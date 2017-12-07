# Load packages
library('dplyr') 
library('data.table')
library('lubridate')
library(ggplot2)

# read data
setwd("D:\\Sun Julie\\University\\Data Science\\SCC460_Data Science Fundamentals\\Groupwork\\THG\\THG-Jarvis")
customers <- read.csv('MAIN_customer_data.csv', stringsAsFactors = F)
# country_code <- read.csv('country_code_lookup.csv', stringsAsFactors = F)
country_code <- read.csv('country_code_lookup.csv')
trans=read.csv("MAIN_transaction_data.csv")


str(customers)
str(country_code)

# number of missing values
sum(is.na(customers))
sum(is.na(PostCode))
# preprocess data
customers <- subset (customers, select=c(1:7)) 
customers$Registered_Date <- as.Date(customers$Registered_Date, format = "%d/%m/%Y")
customers$First_Order_Placed <- as.Date(customers$First_Order_Placed,format = "%d/%m/%Y")
country_code$Country_Code <- gsub("GB", "UK", country_code$Country_Code)

c_code <-data.frame(Country_Code=unique(country_code$Country_Code),
                    Country_Name=unique(country_code$Country_Name))
str(c_code)
c_code$Country_Code=as.character(c_code$Country_Code)
c_code$Country_Name=as.character(c_code$Country_Name)
colnames(customers)[colnames(customers)=="Country"]<- "Country_Name"

# Country_Code
customers<-left_join(customers,c_code, by="Country_Name") 
colnames(customers)[3] <- "Country_Name"
colnames(c_code)

#customers <- c_code %>% 
# left_join(customer)

# join transaction
data <- group_by(trans, Account_Key)     
data <- summarise(data, Order_count=n()) 
customers_main<-left_join(customers,data,by="Account_Key")

chargeback=read.csv("MAIN_chargeback_data.csv")
chargeback$Internal.RC <- gsub("fraud", "Fraud", chargeback$Internal.RC)
colnames(chargeback)[1]<-"Order_Number"
chargeback<-chargeback%>%mutate(fraud_flag=ifelse(chargeback$Internal.RC=="Fraud","1","0"))

# filter out fraud entries
trans_sub <- subset (trans, select=c(1,2,4,13,14)) 

fraud=left_join(chargeback,trans_sub,by="Order_Number")
fraud=left_join(fraud,customers_main,by="Account_Key")
fraud$Order_Date_Key <- ymd(fraud$Order_Date_Key)
fraud1=fraud
fraud=fraud[fraud$Internal.RC=="Fraud",]

fraud1$fraud_flag=as.factor(fraud1$fraud_flag)
fraud1<- group_by(fraud1, Account_Key)
data1<-summarise(fraud1, fraud_flag=n()) 
colnames(data1)[2] <- "fraud_count"

#fraud1=fraud1[fraud1$Internal.RC=="Fraud",]
customers_main<-left_join(customers_main,data1,by="Account_Key")

customers_main<- customers_main%>%mutate(fraud_flag=ifelse(customers_main$fraud_count>0,"1","0"))
#customers_main$fraud_flag=as.numeric(customers_main$fraud_flag)

# create new variables
# Active_years - rough number,probably dynamic calcu is better(using Order_Date_Key)
fraud$Active_years <- round((fraud$Order_Date_Key-fraud$Registered_Date)/365, 1)
fraud$Active_months <- round((fraud$Order_Date_Key-fraud$Registered_Date)/30, 1)
fraud$Active_days <- round((fraud$Order_Date_Key-fraud$Registered_Date), 1)
for (i in 1:nrow(fraud)){
  if ((fraud$Active_months[i]>=0) & (fraud$Active_months[i]<=1)){
    fraud$Active_c[i] = "1"
  }
  else if((fraud$Active_months[i]>1) & (fraud$Active_months[i]<=2)) {
    fraud$Active_c[i] = "2"
  }
  else if((fraud$Active_months[i]>2) & (fraud$Active_months[i]<=3)){
    fraud$Active_c[i] = "3"
  }
  else if((fraud$Active_months[i]>3) & (fraud$Active_months[i]<=4)){
    fraud$Active_c[i] = "4"
  }
  else if((fraud$Active_months[i]>4)& (fraud$Active_months[i]<=5)){
    fraud$Active_c[i] = "5"
  }
  else if((fraud$Active_months[i]>5)& (fraud$Active_months[i]<=6)){
    fraud$Active_c[i] = "6"
  }
  else if((fraud$Active_months[i]>6)& (fraud$Active_months[i]<=7)){
    fraud$Active_c[i] = "7"
  }
  else if((fraud$Active_months[i]>7)& (fraud$Active_months[i]<=8)){
    fraud$Active_c[i] = "8"
  }
  else if((fraud$Active_months[i]>8)& (fraud$Active_months[i]<=9)){
    fraud$Active_c[i] = "9"
  }
  else if((fraud$Active_months[i]>9)& (fraud$Active_months[i]<=10)){
    fraud$Active_c[i] = "10"
  }
  else if((fraud$Active_months[i]>10)& (fraud$Active_months[i]<=11)){
    fraud$Active_c[i] = "11"
  }
  else if((fraud$Active_months[i]>11)& (fraud$Active_months[i]<=12)){
    fraud$Active_c[i] = "12"
  }
  else{
    fraud$Active_c[i] = ">12"
  }
}


# Write the customer data to a new file
write.csv(fraud, file = 'plot1.csv', row.names = F)


# Active_months
p1 <- ggplot(fraud) +
  geom_histogram(aes(x=Active_months),binwidth=1,breaks=seq(0, 15, 1),fill="tan2")
p1
ggsave(p1, file="fraud vs.Active_months.png")

# fraud count per Country code
data2 <- group_by(fraud, Country_Name)
names(data2)
data2 <- summarise(data2, fraud_count=n()) 
sum(is.na(data2$Country_Name))
data2$Country_Name <- as.factor(data2$Country_Name)

names(data2)
p2 <- ggplot(data2,aes(data2$Country_Name,data2$fraud_count)) + 
  geom_bar(stat = 'identity',xlab="Country_Name",fill="steelblue1") +
 labs(xlab = "Country_Name", ylab="fraud_count",title = "fraud count per Country")+ coord_flip() 
p2
ggsave(p2, file="fraud count per Country code.png")

# fraud count per Category_Level_2
data3 <- group_by(fraud, Category_Level_2)     
data3 <- summarise(data3, fraud_count=n()) 
p3 <- ggplot(data3,aes(Category_Level_2,fraud_count),beside=T) + 
  geom_bar(stat = 'identity',aes(fill=factor(Category_Level_2))) + 
  coord_flip() 
p3+geom_area(position='fill')
#+ scale_fill_brewer(palette=c("Set1"))
p3
ggsave(p3, file="fraud count per Category_Level_2.png")

# Category_Level_2, Country code
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                "#C5B4E9", "#E69F00", "#CC21A7", "#EF9E73", "#30EAA2", "#0AB2B2","#009E73",  "#CEB4E9",
                "#E52F00","#E09E73","#CC9E73")

data4 <- group_by(fraud, Country_Name,Category_Level_2)     
data4 <- summarise(data4, fraud_count=n())

p4 <- ggplot(data = data4, mapping = aes(Country_Name, fraud_count, fill = Category_Level_2)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_area(position='fill')+ coord_flip()+
  scale_fill_manual(values=cbbPalette)
p4
ggsave(p4, file="fraud count per Category_Level_Country_code.png")




