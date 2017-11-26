customers=read.csv("MAIN_customer_data.csv")
library(lubridate)

class(customers$Registered_Date)
customers$Registered_Date <- as.Date(customers$Registered_Date, format = "%d/%m/%Y")
customers$First_Order_Placed <- as.Date(customers$First_Order_Placed, format = "%d/%m/%Y")
customers$interval<-interval(customers$Registered_Date, customers$First_Order_Placed)
customers$interval1<-customers$interval/ddays(1)    

library(dplyr)
main=read.csv("MAIN_transaction_data.csv")
main_total<-table(main$Account_Key)
main_total<-data.frame(main_total)
colnames(main_total)<-c("Account_Key","counts")
main_total$Account_Key=as.integer(main_total$Account_Key)
customers_main<-right_join(main_total,customers,by="Account_Key")

frad=read.csv("MAIN_chargeback_data.csv")
colnames(frad)[1]<-"Order_Number"
class(frad$Order_Number)
frad$Order_Number=as.integer(frad$Order_Number)
main_frad=right_join(main,frad,by="Order_Number")
table1=table(main_frad$Account_Key)
table1=data.frame(table1)
colnames(table1)<-c("Account_Key","counts")
table1$Account_Key=as.integer(table1$Account_Key)
customers_frad<-right_join(table1,customers,by="Account_Key")

#merge the whole transaction counts and frad counts together, calculate the proportion
customers_main$countn2<-customers_frad$counts
customers_main$proportaion<-customers_frad$counts/customers_main$countn2

# frad_total<-table(frad$Account_Key)
# frad_total<-data.frame(frad_total)
# colnames(frad_total)<-c("Account_Key","counts")
# frad_total$Account_Key=as.integer(frad_total$Account_Key)
# customers_frad<-right_join(frad_total,customers,by="Account_Key")
