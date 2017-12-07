rm(list = ls())
library(caret)
library(dplyr)
library(corrplot)

setwd("C:/Users/Luke/Documents/University/Lancaster/Data Fundamentals/thgfd/data/stratified")
data <- read.csv("dataset_11.csv")
fraud_data <- read.csv("fraud_data.csv")

glm_data <- rbind(data, fraud_data)
glm_data <- select(glm_data, -Site_Key)

any(duplicated(glm_data))
glm_data <- glm_data[!duplicated(glm_data[,]), ]
any(duplicated(glm_data))

x = ncol(glm_data)

for (i in 1:x){
  if (is.integer(glm_data[,i]) == TRUE){
    glm_data[,i] <- as.factor(glm_data[,i])
  }
}

temp <- select(glm_data, -prop, -canc_prop, -Product_Charge_Price)
temp <- temp[, sapply(temp, nlevels) > 1]
nums <- select(glm_data, prop, canc_prop, Product_Charge_Price)
glm_data <- cbind(temp, nums)

set.seed(123)
index <- createDataPartition(glm_data$fraud_status, p = 0.7, list = FALSE)
train <- glm_data[index, ]
test <- glm_data[-index, ]

# Attempting PCA
# pca_factor_data <- glm_data
# pca_factor_data <- select(pca_factor_data, -fraud_status, -canc_prop, -prop, -Product_Charge_Price, -Ordered_Product_Key, -Campaign_Key)
#
# for (i in 1:ncol(pca_factor_data)){
#   print(any(!is.finite(pca_factor_data[,i])))
# }
#
# fraud.pca <- prcomp(pca_factor_data, center = TRUE, scale. = TRUE)
# fraud.pca2 <- princomp(pca_factor_data, cor = T)



# Back to GLM with reduced glm_data data frame
train <- select(train, -Campaign_Key, -Ordered_Product_Key)

train_test <- select(train, -(1:19), -pay_key_num, -Cancelled_Qty, -count, -Order_Payment_Status_Key, -destination_int, -priority_int, -occupation_int, -Delivery_Option_Type_Key, -num_valid, -Order_Sequence_No)

train_test_2 <- select(train, -pay_key_num, -Cancelled_Qty, -count, -Locale_Key, -Order_Payment_Status_Key, -destination_int, -priority_int, -occupation_int, -Delivery_Option_Type_Key, -num_valid, -Order_Sequence_No)

train_test_3 <- select(train, (1:19), fraud_status)

start <- Sys.time()
model <- glm(fraud_status ~., data = train_test, family = binomial(link = "logit"))
end <- Sys.time()
time <- end - start
time
summary(model)

model_reduc <- step(model)
summary(model_reduc)

# unique(table(glm_data$Category_Level_2Footwear))[2]
