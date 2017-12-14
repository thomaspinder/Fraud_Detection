rm(list = ls())
library(caret)
library(dplyr)
library(corrplot)
library(DMwR)
library(unbalanced)
library(ROCR)

setwd("C:/Users/Luke/Documents/University/Lancaster/Data Fundamentals/thgfd/data/stratified")
glm_data <- read.csv("dataset_11.csv")
glm_data$fraud_status <- as.factor((glm_data$fraud_status))
glm_data <- select(glm_data, -Site_Key, -Ordered_Product_Key)

set.seed(123)
Tomek <- ubTomek(select(glm_data, -fraud_status), glm_data$fraud_status, verbose = TRUE)
glm_data <- cbind(Tomek$X, Tomek$Y)
glm_data <- rename(glm_data, fraud_status = "Tomek$Y")

temp <- select(glm_data, -prop, -canc_prop, -Product_Charge_Price, -num_valid, 
               -count, -Order_Sequence_No, -Ordered_Qty, -Cancelled_Qty)
temp[,(1:ncol(temp))] <- lapply(temp[,(1:ncol(temp))],as.factor)
temp <- temp[, sapply(temp, nlevels) > 1]
nums <- select(glm_data, prop, canc_prop, Product_Charge_Price, num_valid, 
               count, Order_Sequence_No, Ordered_Qty, Cancelled_Qty)
glm_data <- cbind(temp, nums)

glm_data <- select(glm_data, -Campaign_Key, -Delivery_Option_Type_Key, -Payment_Method_Key,
                   -Medium_Key, -pay_key_num, -Payment_Provider_Key, 
                   -Order_Payment_Status_Key, -occupation_int,
                   -canc_prop, -Ordered_Qty, -priority_int, -Cancelled_Qty, -prop,
                   -Order_Sequence_No, -count)

glm_data <- select(glm_data, -Category_Level_2Accessories, 
                   -Category_Level_2Bags, -Category_Level_2Gifting)

new_data <- glm_data

set.seed(123)
index <- createDataPartition(glm_data$fraud_status, p = 0.7, list = FALSE)
train <- new_data[index, ]
test <- new_data[-index, ]

set.seed(123)
train <- SMOTE(fraud_status ~., new_data, perc.over = 1000, perc.under = 150)
table(new_data$fraud_status)
prop.table(table(new_data$fraud_status))

# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~., data = train, family = binomial(link = "logit"))
end <- Sys.time()
time <- end - start
time
summary(model)

model <- step(model)
summary(model)

model <- update(model, .~. -Category_Level_2Clothing-Category_Level_2Footwear)
summary(model)

#Plotting an ROC curve
model_predict <- predict(model, test, type = "response")
ROC_predict <- prediction(model_predict, test$fraud_status)
ROC_performance <- performance(ROC_predict, "tpr", "fpr")
plot(ROC_performance, colorize = TRUE, text.adj = c(-0.2,1.7), lwd = 5)
area_under_curve <- performance(ROC_predict, measure = "auc")
area_under_curve@y.values[[1]]

# TEN FOLD CROSS VALIDATION
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- caret::train(fraud_status ~ customer_status + ship_status + destination_int + 
                     Product_Charge_Price + num_valid,
                   data = train, method = "glm", family = binomial(link ="logit"),
                   trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = select(test, -fraud_status))
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass