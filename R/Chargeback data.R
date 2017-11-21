---
title: "Chargeback Feature Exploration"
author: "Omar And Luke"
date: "21 November 2017"
output: html_document
---

rm(list = ls())
library(lubridate)
setwd("H:/SCC.460 Data Science Fundamentals/Project/THG-Jarvis")
data <- read.csv("MAIN_chargeback_data.csv")

#' Cleaning the data.
data$GBP.Amount <- gsub("£", "", data$GBP.Amount)
data$GBP.Amount <- as.numeric(data$GBP.Amount)
data$Date.Logged <- as.Date(data$Date.Logged, format = "%d/%m/%Y")

str(data)
class(data$Date.Logged)
year(data$Date.Logged[2])

data$Internal.RC <- gsub("fraud", "Fraud", data$Internal.RC)
data$Internal.RC <- gsub("untracked", "Untracked", data$Internal.RC)
data$Internal.RC <- gsub("error", "Error", data$Internal.RC)
data$Internal.RC <- as.factor(data$Internal.RC)

#' Creating a seasons variable.
data$Season <- NA
for (i in 1:length(data$Date.Logged)){
  if (month(data$Date.Logged[i]) == 12 | 
      month(data$Date.Logged[i]) == 1 | 
      month(data$Date.Logged[i]) == 2){
    data$Season[i] <- "Winter"
  }
  else if (month(data$Date.Logged[i]) == 3 |
           month(data$Date.Logged[i]) == 4 |
           month(data$Date.Logged[i]) == 5){
    data$Season[i] <- "Spring"
  }
  else if (month(data$Date.Logged[i]) == 6 |
           month(data$Date.Logged[i]) == 7 |
           month(data$Date.Logged[i]) == 8){
    data$Season[i] <- "Summer"
  }
  else if (month(data$Date.Logged[i]) == 9 |
           month(data$Date.Logged[i]) == 10 |
           month(data$Date.Logged[i]) == 11){
    data$Season[i] <- "Autumn"
  }
}
data$Season <- as.factor(data$Season)

#' Creating a month column.
data$Month <- month(data$Date.Logged, label = TRUE, abbr = TRUE)




