library(dplyr)
setwd("~/Desktop/DataScienceFundamentals_Project/thgfd/data")
data <- read.csv("notFinished_transactions_updated.csv", stringsAsFactors=FALSE)
attach(data)

sum(is.na(data))
# 417450 entries are NA
data <- select(data, -Date.Logged)
sum(is.na(data))
# Now there are 266 entries that are NA
data[is.na(data)] <- -1
sum(is.na(data))
write.csv(data, "updated_file.csv")

#data <- with(data, data.frame(model.matrix(~Category_Level_2 -1), data))
#data <- with(data, data.frame(model.matrix(~Category_Level_3 -1), data))





