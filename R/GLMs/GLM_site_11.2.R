rm(list = ls())
library(caret)
library(dplyr)
library(DMwR)
library(unbalanced)
library(ROCR)

setwd("C:/Users/Luke/Documents/University/Lancaster/Data Fundamentals/thgfd/data/stratified")
glm_data <- read.csv("dataset_11.csv")
glm_data$fraud_status <- as.factor((glm_data$fraud_status))
glm_data <- select(glm_data, -Site_Key)


set.seed(123)
Tomek <- ubTomek(select(glm_data, -fraud_status), glm_data$fraud_status, verbose = TRUE)
glm_data <- cbind(Tomek$X, Tomek$Y)
glm_data <- rename(glm_data, fraud_status = "Tomek$Y")




glm_data <- select(glm_data, Product_Charge_Price, prop,
                   count, num_valid, Cancelled_Qty, canc_prop,
                   customer_status, Order_Sequence_No, fraud_status)



glm_data$customer_status <- as.factor(glm_data$customer_status)




new_data <- glm_data


set.seed(123)
index <- createDataPartition(glm_data$fraud_status, p = 0.7, list = FALSE)
train <- new_data[index, ]
test <- new_data[-index, ]



set.seed(123)
train <- SMOTE(fraud_status ~., new_data, perc.over = 1000, perc.under = 150)
table(train$fraud_status)
prop.table(table(train$fraud_status))



# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~., data = train, family = binomial(link = "logit"))
end <- Sys.time()
time <- end - start
time
summary(model)


model_predict <- predict(model, test, type = "response")
ROC_predict <- prediction(model_predict, test$fraud_status)
ROC_performance <- performance(ROC_predict, "tpr", "fpr")
plot(ROC_performance, colorize = TRUE, text.adj = c(-0.2,1.7), lwd = 5)
area_under_curve <- performance(ROC_predict, measure = "auc")
area_under_curve@y.values[[1]]



# TEN FOLD CROSS VALIDATION
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model_fit <- caret::train(fraud_status ~ .,
                   data = train, method = "glm", family = binomial(link ="logit"),
                   trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = test)
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass
