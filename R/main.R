library(data.table)
library(mice)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/DataScienceFundamentals_Project/thgfd/data")
trans <- fread("MAIN_transaction_data.csv")

# 
str(trans)
unique(trans$Empty1)

del_key <- fread("delivery_option_lookup.csv")
colnames(del_key) <- c("Delivery_Option_Type_Key", "Name")

trans_del <- trans %>% 
  left_join(del_key) %>% 
  group_by(Name) %>% 
  summarise(n = n()) 

trans <- trans %>% 
  left_join(del_key)

str(del_key)
del_features <- data.frame(Name = as.character(unique(trans$Name)), 
                           destination = c(rep("int", 3), rep("dom", 6)),
                           priority = c("priority", rep("slow", 2), rep("priority", 2), rep("slow", 4)),
                           occupation = c(rep("unknown", 5), "working", "unknown", "working", "unknown"))

trans <- trans %>% 
  left_join(del_features, by = "Name")

trans_del <- trans %>% 
  group_by(Name) %>% 
  summarise(n = n())

trans_del %>% 
  ggplot(aes(x = Name, y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
head(trans)
# Reliable customers
customers <- trans %>% 
  group_by(Account_Key) %>% 
  mutate(count = n()) %>% 
  group_by(count) %>% 
  summarise(n = n()) 

customers$prop <- customers$n/dim(trans)[1]  
customers$cumulate <- cumsum(customers$prop)
customers %>% 
  ggplot(aes(x = count, y = cumulate)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 800, by = 25)) +
  scale_y_continuous(breaks = seq(0, 800, by = 0.1)) +
  theme_minimal()

payment_hists <- trans %>% 
  group_by(Account_Key) %>% 
  mutate(pay_key_num = as.numeric(Order_Payment_Status_Key),
         num_valid = length(which(pay_key_num == 0)),
         count = n(),
         prop = num_valid/count,
         status = ifelse(prop <= 0.25 & count > 2, "questionable",
                         ifelse(prop <= 0.25 & count <= 2, "insufficient",
                                ifelse(prop > 0.25 & prop < 0.75, "adequate", "fine"))))

cancelled_items <- trans %>% 
  group_by(Category_Level_3, Category_Level_2) %>% 
  mutate(canc_prop = ifelse(Cancelled_Qty == 0, 0, Cancelled_Qty/Ordered_Qty))

hist(as.numeric(trans$Order_Payment_Status_Key))

bill_ship_match <- trans %>% 
  mutate(status = ifelse(Postcode_Billing_Address != Postcode_Shipping_Address, "mismatch", "fine"))

bill_ship_match %>% 
  ggplot(aes(x = status)) +
  geom_bar(stat = "count")
table(bill_ship_match$status)
