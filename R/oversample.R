library(ROSE)
library(data.table)
library(rpart)
library(dplyr)

setwd("Documents/university/MSc/fundamental/fraud/data/")
chargeback <- fread("MAIN_chargeback_data.csv")
colnames(chargeback)[1] <- "Order_Number"
trans <- fread("MAIN_transaction_data.csv")

to_model <- trans %>% 
  left_join(chargeback, "Order_Number")

to_model$fraud_status <- ifelse(is.na(to_model$Internal.RC), 0, 1)
table(to_model$fraud_status)

balanced <- ovun.sample(fraud_status ~ Product_Charge_Price, method = "both", 
                        N = dim(to_model)[1]*1.5, data = to_model, seed = 123)$data
table(balanced$fraud_status)