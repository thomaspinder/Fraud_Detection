library(caret)
library(data.table)
library(randomForest)
library(mice)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ROSE)
setwd("~")
setwd("Documents/university/MSc/fundamental/fraud/data/")
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
  mutate(ship_status = ifelse(Postcode_Billing_Address != Postcode_Shipping_Address, "mismatch", "fine"))

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
names(trans_noNa)
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
to_model$Site_Key <- as.factor(to_model$Site_Key)

# Filter out unecessary columns
to_model2 <- to_model %>% 
  select_(.dots = names(x))

to_model3 <- to_model2 %>% 
  select_(.dots = union(intersect(names(to_model2), names(y)), names(trans_noNa)[26]))

to_model3 <- to_model3 %>% 
  select(-c(Order_Date_Key, Cancelled_Date_Key))

setwd("~")
setwd("Documents/university/MSc/fundamental/fraud/data/stratified/")

site_ks <- unique(to_model3$Site_Key)

for (i in 1:length(site_ks)){
  print(as.character(site_ks[i]))
  data <- to_model3 %>% 
    filter(Site_Key == site_ks[i])
  write.csv(data, row.names = F , file = paste0("stratified/dataset_", as.character(site_ks[i]), ".csv"))
}
setwd("~")
fraud_data <- to_model3 %>% 
  filter(fraud_status == 1)
write.csv(fraud_data, file = "Documents/university/MSc/fundamental/fraud/data/stratified/fraud_data.csv")
