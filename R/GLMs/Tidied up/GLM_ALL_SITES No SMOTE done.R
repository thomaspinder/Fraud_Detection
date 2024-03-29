rm(list = ls())
library(caret)
library(dplyr)
library(DMwR)
library(unbalanced)
library(ROCR)

setwd("C:/Users/Luke/Documents/University/Lancaster/Data Fundamentals/thgfd/data/stratified")
glm_data <- read.csv("ALL_Data.csv")
glm_data <- select(glm_data, -Ordered_Product_Key)

glm_data <- filter(glm_data, Site_Key!='119')
glm_data <- filter(glm_data, Site_Key!='120')

#Removing zero variance variables
glm_data <- Filter(function(x) var(x)!=0, glm_data)

glm_data$fraud_status <- as.factor((glm_data$fraud_status))

glm_data <- select(glm_data, -(1:28), -Campaign_Key, -Ordered_Qty, -Cancelled_Qty, -prop,
                   -canc_prop, -count, -num_valid)

set.seed(123)
Tomek <- ubTomek(select(glm_data, -fraud_status), glm_data$fraud_status, verbose = TRUE)
glm_data <- cbind(Tomek$X, Tomek$Y)
glm_data <- rename(glm_data, fraud_status = "Tomek$Y")

# After building GLM
glm_data <- select(glm_data, -Order_Sequence_No, -Payment_Method_Key, -pay_key_num,
                   -Medium_Key, -Delivery_Option_Type_Key, -Order_Payment_Status_Key,
                   -Payment_Provider_Key, -occupation_int, -Locale_Key)

# Turn into factors
temp <- select(glm_data, -Product_Charge_Price)
temp[,(1:ncol(temp))] <- lapply(temp[,(1:ncol(temp))],as.factor)
nums <- select(glm_data, Product_Charge_Price)
glm_data <- cbind(temp, nums)

new_data <- glm_data

set.seed(123)
index <- createDataPartition(new_data$fraud_status, p = 0.6, list = FALSE)
train <- new_data[index, ]
test <- new_data[-index, ]

# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~., data = train, family = binomial(link = "logit"))
end <- Sys.time()
time <- end - start
time
summary(model)

# Plotting an ROC curve
model_predict <- predict(model, test, type = "response")
ROC_predict <- prediction(model_predict, test$fraud_status)
ROC_performance <- performance(ROC_predict, "tpr", "fpr")
plot(ROC_performance, colorize = TRUE, text.adj = c(-0.2,1.7), lwd = 5)
area_under_curve <- performance(ROC_predict, measure = "auc")
area_under_curve@y.values[[1]]

# TEN FOLD CROSS VALIDATION
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- caret::train(fraud_status ~.,
                          data = train, method = "glm", family = binomial(link ="logit"),
                          trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = test)
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass