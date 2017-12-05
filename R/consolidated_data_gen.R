library(caret)
library(data.table)
library(randomForest)
library(mice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ROSE)

#setwd("Documents/university/MSc/fundamental/fraud/data/")
trans <- fread("MAIN_transaction_data.csv")
fraud_trans <- fread("MAIN_chargeback_data.csv")
customers <- read.csv("MAIN_customer_data.csv")

del_key <- fread("delivery_option_lookup.csv")
colnames(del_key) <- c("Delivery_Option_Type_Key", "Name")

trans_del <- trans %>% 
  left_join(del_key) %>% 
  group_by(Name) %>% 
  summarise(n = n()) 

trans <- trans %>% 
  left_join(del_key)

del_features <- data.frame(Name = as.character(unique(trans$Name)), 
                           destination = c(rep("int", 3), rep("dom", 6)),
                           priority = c("priority", rep("slow", 2), rep("priority", 2), rep("slow", 4)),
                           occupation = c(rep("unknown", 5), "working", "unknown", "working", "unknown"))

trans <- trans %>% 
  left_join(del_features, by = "Name") 
trans_del <- trans %>% 
  group_by(Name) %>% 
  summarise(n = n())

trans <- trans %>% 
  left_join(customers[c("Account_Key", "Site_Key")], by = "Account_Key")
trans$Site_Key <-as.character(trans$Site_Key)
trans$Site_Key_len <- nchar(trans$Site_Key)

trans <- trans %>% 
  filter(Site_Key_len <5) %>% 
  mutate(Site_Key = as.factor(Site_Key)) %>% 
  select(-Site_Key_len)

# Reliable customers
customers <- trans %>% 
  group_by(Account_Key) %>% 
  mutate(count = n()) %>% 
  group_by(count) %>% 
  summarise(n = n()) 

customers <- trans %>% 
  group_by(Account_Key) %>% 
  mutate(count = n()) %>% 
  group_by(count) %>% 
  summarise(n = n()) 

customers$prop <- customers$n/dim(trans)[1]  
customers$cumulate <- cumsum(customers$prop)

payment_hists <- trans %>% 
  group_by(Account_Key) %>% 
  mutate(pay_key_num = as.numeric(Order_Payment_Status_Key),
         num_valid = length(which(pay_key_num == 0)),
         count = n(),
         prop = num_valid/count,
         status = ifelse(prop <= 0.25 & count > 2, "questionable",
                         ifelse(prop <= 0.25 & count <= 2, "insufficient",
                                ifelse(prop > 0.25 & prop < 0.75, "adequate", "fine"))))

cancelled_items <- payment_hists %>% 
  group_by(Category_Level_3, Category_Level_2) %>% 
  mutate(canc_prop = ifelse(Cancelled_Qty == 0, 0, Cancelled_Qty/Ordered_Qty))


bill_ship_match <- cancelled_items %>% 
  mutate(status = ifelse(Postcode_Billing_Address != Postcode_Shipping_Address, "mismatch", "fine"))

trans <- bill_ship_match

trans$Order_Date_Key <- ymd(trans$Order_Date_Key)
fraud_accounts <- customers
colnames(fraud_trans)[1] <- "Order_Number"

fraud <- fraud_trans %>% 
  inner_join(trans, by = "Order_Number")

to_app <- fraud_trans %>% 
  right_join(trans, by = "Order_Number") %>% 
  filter(is.na(Internal.RC) == TRUE)

trans <- rbindlist(list(to_app, fraud))
trans$fraud_status <- ifelse(is.na(trans$Internal.RC) == TRUE, 0, 1)

trans <- select(trans,-c(Order_Number, Released_by, Internal.RC, Defence.Status, GBP.Amount, Account_Key, Empty1, Empty2))

trans_noNa <- trans %>% 
  select(-Date.Logged)

# Columns with NAs
na.count <- which(sapply(trans_noNa, function(y) sum(length(which(is.na(y)))))>0)

library(VIM)
aggr_plot <- aggr(trans_noNa, col = c("blue", "red"), numbers = TRUE, sortVars = TRUE, 
                  labels=names(trans_noNa), cex.axis = 7, gap = 3, 
                  ylab = c("Histogram of Missing Data", "Pattern"))

# Are nas fraudulent
na.count <- which(sapply(trans_noNa[trans_noNa$fraud_status==1], function(y) sum(length(which(is.na(y)))))>0)
sum(is.na(trans_noNa[trans_noNa$fraud_status==1]))

# Remove NAs as they're non-fraudulent
to_model <- trans_noNa %>% 
  na.omit()

# One-Hot Encode Variables
# Returns 208 Features
to_model <- with(to_model, data.frame(model.matrix(~Category_Level_2 -1), to_model))

# Run Random Forest
#setwd("/home/tpin3694/Documents/university/MSc/fundamental")

x <- to_model[, sapply(to_model, class) != "character"]
y <- to_model[, sapply(to_model, class) != "factor"]

#1=domestic, 2=international
to_model$destination_int <- as.integer(to_model$destination)
# 1=priority, 2=slow
to_model$priority_int <- as.integer(to_model$priority)
# 1=unknown, 2=working
to_model$occupation_int <- as.integer(to_model$occupation)
#1=fine, 2=mismatch
to_model$customer_status_int <- as.integer(as.factor(to_model$status))

# Filter out unecessary columns
to_model2 <- to_model %>% 
  select_(.dots = names(x))
to_model3 <- to_model2 %>% 
  select_(.dots = intersect(names(to_model2), names(y)))

#write.csv(to_model3, "hot_pot_encoded.csv", row.names = FALSE)
library(randomForest)

# scale down dataset
set.seed(123)
to_model3$fraud_status <- as.factor(to_model3$fraud_status)
summary(to_model3$fraud_status)

# What sites do we have
unique(to_model3)

# Split into test/train
set.seed(123)
index <- createDataPartition(to_model3$fraud_status, p = 0.7, list = FALSE)
train_data <- to_model3[index, ]
test_data <- to_model3[-index, ]

# Random Remove 50% of fraud
row_indexes <- which(train_data$fraud_status == 0)
sub_data <- sample(x = row_indexes, size = nrow(train_data)*0.5)
# Setup Random Forest
model_rf <- train(fraud_status ~ ., 
                  data = train_data[-sub_data, ],
                  preProcess = c("scale", "center"),
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 10,
                                           verboseIter = TRUE))
final <- data.frame(actual = test_data$fraud_status, 
                    predict(model_rf, newdata = test_data, type = "prob"))
# final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
# cm_original <- confusionMatrix(final$predict, test_data$classes)

# Under Sample now
ctrl <- trainControl(method = "repeatedcv", 
                     number = 2, 
                     repeats = 2,#10 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(123)
nmin <- sum(train_data$fraud_status == 1)
model_rf_under <- train(fraud_status ~ .,
                        data = train_data,
                        method = "rf",
                        preProcess = c("scale", "center"),
                        trControl = ctrl,
                        strata = train_data$fraud_status,
                        sampsize = rep(nmin, 2))

final_under <- data.frame(actual = test_data$fraud_status, 
                          predict(model_rf_under, newdata = test_data, 
                                  type = "prob"))

final_under$predict <- ifelse(final_under$X0 < 0.5, 0, 1)
cm_under <- confusionMatrix(final_under$predict, test_data$fraud_status)
cm_under
# Plot Model
plot(rf_model, ylim = c(0, 0.99))
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
plot(cm_under$table)
cm_under$table
