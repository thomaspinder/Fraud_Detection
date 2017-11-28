library(lubridate)
library(dplyr)
library(ggplot2)

#
customers=read.csv("customer-new.csv")
trans=read.csv("MAIN_transaction_data.csv")

data <- group_by(trans, Account_Key)     
data <- summarise(data, Order_count=n()) 
customers_main<-left_join(customers,data,by="Account_Key")

chargeback=read.csv("MAIN_chargeback_data.csv")
chargeback$Internal.RC <- gsub("fraud", "Fraud", chargeback$Internal.RC)
colnames(chargeback)[1]<-"Order_Number"

# filter out fraud entries
trans_sub <-data.frame(Order_Number=(trans$Order_Number),
                    Account_Key=(trans$Account_Key))

fraud=left_join(chargeback,trans_sub,by="Order_Number")
fraud=left_join(fraud,customers_main,by="Account_Key")
fraud=fraud[fraud$Internal.RC=="Fraud",]

ggplot(fraud) +
  geom_histogram(aes(x=fraud$Joined_years), breaks=seq(0, 10, 1)) 

data1 <- group_by(fraud, Joined_cat,Internal.RC)     
data1 <- summarise(data1, count=n()) 
barplot(data1$count,names=data1$Joined_cat,ylab="count",
        main="count by Joined_cat",ylim=c(0,750))

data2 <- group_by(fraud, Country_Code)     
data2 <- summarise(data2, fraud_count=n()) 
barplot(data2$fraud_count,names=data2$Country_Code,ylab="count_fraud",
        main="count by country code",ylim=c(0,350))


data3 <- group_by(fraud,fraud$Account_Key)
data3 <- summarise(data3, fraud_count=n()) 
colnames(data3)[colnames(data3)=="fraud$Account_Key"]<- "Account_Key"

customers_main<-left_join(customers_main,data3,by="Account_Key")
customers_main<-customers_main%>%mutate(fraud_flag=ifelse(customers_main$fraud_count>0,"1","0"))



