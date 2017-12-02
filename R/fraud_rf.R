library(data.table)
library(dplyr)
library(randomForest)
library(ggplot2)
setwd("C:/Users/Luke/Documents/University/Lancaster/Data Fundamentals/thgfd/data")
trans <- fread("MAIN_transaction_data.csv")
fraud_trans <- fread("MAIN_chargeback_data.csv")
customers <- read.csv("MAIN_customer_data.csv")

colnames(chargeback)
colnames(fraud_trans)
colnames(fraud_trans)[1] <- "Order_Number"
colnames(customers)

chargeback$Order_Date_Key <- ymd(chargeback$Order_Date_Key)

samp <- runif(n = 0.01*nrow(chargeback), min = 1, max = dim(chargeback)[1])

fraud_accounts <- customers

fraud <- fraud_trans %>% 
  inner_join(chargeback, by = "Order_Number")

to_app <- fraud_trans %>% 
  right_join(chargeback, by = "Order_Number") %>% 
  filter(is.na(Internal.RC) == TRUE) %>% 
  sample_n(size = nrow(chargeback)*0.01)

# 1 indicates fraud, 0 not legitimate
train <- rbindlist(list(to_app, fraud))
train$fraud_status <- ifelse(is.na(train$Internal.RC) == TRUE, 0, 1)

del_key <- fread("delivery_option_lookup.csv")
colnames(del_key) <- c("Delivery_Option_Type_Key", "Delivery_Name")
trans_del <- trans %>% 
  left_join(del_key) %>% 
  group_by(Delivery_Name) %>% 
  summarise(n = n()) 

train <- train %>% 
  left_join(del_key, by = "Delivery_Option_Type_Key")

train <- train %>% 
  group_by(Account_Key) %>% 
  mutate(pay_key_num = as.numeric(Order_Payment_Status_Key),
         num_valid = length(which(pay_key_num == 0)),
         count = n(),
         prop = num_valid/count,
         status = ifelse(prop <= 0.25 & count > 2, "questionable",
                         ifelse(prop <= 0.25 & count <= 2, "insufficient",
                                ifelse(prop > 0.25 & prop < 0.75, "adequate", "fine"))))

train <- train %>% 
  mutate(ship_status = ifelse(Postcode_Billing_Address != Postcode_Shipping_Address, "mismatch", "fine")) %>% 
  group_by(Category_Level_3, Category_Level_2) %>% 
  mutate(canc_prop = ifelse(Cancelled_Qty == 0, 0, Cancelled_Qty/Ordered_Qty))

library(lubridate)
train$Day <- wday(train$Date.Logged, label = TRUE, abbr = TRUE)
weekday <- c("Mon", "Tue", "Wed", "Thu", "Fri")
train$Weekday <- 0
for (i in 1:length(train$Day)){
  if (any(weekday==train$Day[i]) == TRUE){
    train$Weekday[i] = 1
  }
  else{
    train$Weekday[i] = 0
  }
}

# Remove unecessary columns
train <- train %>% 
  select(-c(Order_Number, Released_by, Internal.RC, Defence.Status, GBP.Amount, Account_Key, Empty1, Empty2))

# Build model
attach(train)
rf_model <- randomForest(factor(fraud_status) ~ Delivery_Option_Type_Key + Payment_Method_Key + Payment_Provider_Key +
                           Ordered_Qty + Product_Charge_Price + factor(status) + canc_prop +
                           factor(Weekday))
detach(train)
plot(rf_model, ylim = c(0, 0.5))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Variable Importance
var_importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(var_importance), 
                            Importance = round(var_importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_minimal()

