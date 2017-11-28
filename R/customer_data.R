# Load packages
library('dplyr') 
library('data.table')
library('lubridate')

# read data
setwd("D:\\Sun Julie\\University\\Data Science\\SCC460_Data Science Fundamentals\\Groupwork\\THG\\THG-Jarvis")
customer <- read.csv('MAIN_customer_data.csv', stringsAsFactors = F)
country_code <- read.csv('country_code_lookup.csv', stringsAsFactors = F)

str(customer)
str(country_code)

# number of missing values
sum(is.na(customer))
sum(is.na(PostCode))

# preprocess data
customer$Registered_Date <- as.Date(customer$Registered_Date, format = "%d/%m/%Y")
customer$First_Order_Placed <- as.Date(customer$First_Order_Placed,format = "%d/%m/%Y")
country_code$Country_Code <- gsub("GB", "UK", country_code$Country_Code)

c_code <-data.frame(Country_Code=unique(country_code$Country_Code),
                    Country_Name=unique(country_code$Country_Name))
str(c_code)
c_code$Country_Code=as.character(c_code$Country_Code)
c_code$Country_Name=as.character(c_code$Country_Name)

colnames(customer)[colnames(customer)=="Country"]<- "Country_Name"
#customer$Country_Name= factor(customer$Country_Name)

#customer_new <- c_code %>% 
# left_join(customer)

customer<-left_join(customer,c_code,by.x=customer$Country_Name,by.y=c_code$Country_Name) # Country_Code
#customer_new<-right_join(c_code,customer,by.x=customer$Country_Name,by.y=c_code$Country_Name) # Country_Code

# create new variables
# Joined_years
customer$Joined_years <- round((dmy("31/12/2016")-customer$Registered_Date)/365, 1)
for (i in 1:nrow(customer)){
  if ((customer$Joined_years[i]>=0) & (customer$Joined_years[i]<1)){
    customer$Joined_cat[i] = "[0,1]"
  }
  else if((customer$Joined_years[i]>=1) & (customer$Joined_years[i]<3)) {
    customer$Joined_cat[i] = "[1,3]"
  }
  else if((customer$Joined_years[i]>=3) & (customer$Joined_years[i]<5)){
    customer$Joined_cat[i] = "[3,5]"
  }
  else if((customer$Joined_years[i]>=5) & (customer$Joined_years[i]<10)){
    customer$Joined_cat[i] = "[5,10]"
  }
  else if((customer$Joined_years[i]>=10)){
    customer$Joined_cat[i] = "[10,999]"
  }
  else{
    customer$Joined_cat[i] = "NA"
  }
}


# Write the customer data to a new file
write.csv(customer, file = 'customer-new.csv', row.names = F)














