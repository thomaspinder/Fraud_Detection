library(data.table)
library(mice)
library(dplyr)
library(ggplot2)
library(lubridate)

setwd("~/Desktop/DataScienceFundamentals_Project/thgfd/data")
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
write.csv(trans, "notFinished_transactions_updated.csv")

