### First we start by clearing the global environment and loading the required libraries.

rm(list = ls())
library(caret)
library(dplyr)
library(corrplot)
library(DMwR)
library(unbalanced)
library(ROCR)



### Next we build a GLM for site 11 using resampling techniques.

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



### Next we build a GLM for site 11 without resampling tehniques

glm_data <- read.csv("dataset_11.csv")
glm_data$fraud_status <- as.factor((glm_data$fraud_status))
glm_data <- select(glm_data, -Site_Key, -Ordered_Product_Key)

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

# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~., data = train, family = binomial(link = "logit"))
end <- Sys.time()
time <- end - start
time
summary(model)

model <- step(model)
summary(model)

model <- update(model, .~. -Category_Level_2Clothing)
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
                            Category_Level_2Clothing + num_valid ,
                          data = train, method = "glm", family = binomial(link ="logit"),
                          trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = select(test, -fraud_status))
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass



### Now we build a GLM for site 15 using resampling techniques

glm_data <- read.csv("dataset_15.csv")
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

glm_data <- select(glm_data, -Category_Level_2Accessories, -Category_Level_2Beauty.Boxes,
                   -Category_Level_2Books, -Category_Level_2Gifting, -Category_Level_2Gifts,
                   -Category_Level_2Health.and.Beauty.Gift.Sets, -Category_Level_2Homeware,
                   -Category_Level_2Nail.Care, -Category_Level_2Shaving, 
                   -Category_Level_2Sports.Nutrition, -Category_Level_2Fragrance,
                   -Category_Level_2Hair.Care, -Category_Level_2Bags)

new_data <- glm_data

set.seed(123)
index <- createDataPartition(new_data$fraud_status, p = 0.7, list = FALSE)
train <- new_data[index, ]
test <- new_data[-index, ]

set.seed(123)
train <- SMOTE(fraud_status ~., train, perc.over = 1000, perc.under = 150)
table(new_data$fraud_status)
prop.table(table(new_data$fraud_status))

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
model_fit <- caret::train(fraud_status ~ Category_Level_2Skin.Care + Category_Level_2Make.Up +
                            Category_Level_2Health.and.Beauty.Electricals + 
                            customer_status + Category_Level_2Body.Care +
                            ship_status + destination_int + Product_Charge_Price + num_valid,
                          data = train, method = "glm", family = binomial(link ="logit"),
                          trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = test)
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass



### Now we build a GLM for site 121 using resampling techniques

glm_data <- read.csv("dataset_121.csv")

glm_data <- select(glm_data, -Site_Key, -Ordered_Product_Key)

#Removing zero variance variables
glm_data <- Filter(function(x) var(x)!=0, glm_data)

glm_data$fraud_status <- as.factor((glm_data$fraud_status))

glm_data <- select(glm_data, -(1:24), -Campaign_Key, -Ordered_Qty, -Cancelled_Qty, -prop,
                   -canc_prop, -count)

# After building GLM
glm_data <- select(glm_data, -Medium_Key, -pay_key_num, -Payment_Provider_Key,
                   -Payment_Method_Key, -Delivery_Option_Type_Key, -Order_Payment_Status_Key,
                   -num_valid, -Order_Sequence_No)

set.seed(123)
Tomek <- ubTomek(select(glm_data, -fraud_status), glm_data$fraud_status, verbose = TRUE)
glm_data <- cbind(Tomek$X, Tomek$Y)
glm_data <- rename(glm_data, fraud_status = "Tomek$Y")

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

set.seed(123)
train <- SMOTE(fraud_status ~. , train, perc.over = 1000, perc.under = 150)
table(train$fraud_status)
prop.table(table(train$fraud_status))

# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~.-1, data = train, family = binomial(link = "logit"))
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
model_fit <- caret::train(fraud_status ~ customer_status + Product_Charge_Price +
                            ship_status + destination_int +
                            priority_int + occupation_int,
                          data = train, method = "glm", family = binomial(link ="logit"),
                          trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = test)
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass



### Now we build a GLM for site 153 using resampling techniques

glm_data <- read.csv("dataset_153.csv")

glm_data <- select(glm_data, -Site_Key, -Ordered_Product_Key)

#Removing zero variance variables
glm_data <- Filter(function(x) var(x)!=0, glm_data)

glm_data$fraud_status <- as.factor((glm_data$fraud_status))

glm_data <- select(glm_data, -(1:19), -Campaign_Key, -Ordered_Qty, -Cancelled_Qty, -prop,
                   -canc_prop, -count)

# After building GLM
glm_data <- select(glm_data, -Medium_Key, -pay_key_num, -Payment_Provider_Key,
                   -Payment_Method_Key, -Delivery_Option_Type_Key, -Order_Payment_Status_Key,
                   -num_valid, -Order_Sequence_No, -priority_int)

set.seed(123)
Tomek <- ubTomek(select(glm_data, -fraud_status), glm_data$fraud_status, verbose = TRUE)
glm_data <- cbind(Tomek$X, Tomek$Y)
glm_data <- rename(glm_data, fraud_status = "Tomek$Y")

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

set.seed(123)
train <- SMOTE(fraud_status ~., train, perc.over = 1000, perc.under = 150)
table(train$fraud_status)
prop.table(table(train$fraud_status))

# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~.-1, data = train, family = binomial(link = "logit"))
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
model_fit <- caret::train(fraud_status ~ customer_status + Product_Charge_Price +
                            ship_status + destination_int + Locale_Key,
                          data = train, method = "glm", family = binomial(link ="logit"),
                          trControl = ctrl, tuneLength = 10)
pred <- predict(model_fit, newdata = test)
conf <- confusionMatrix(data = pred, test$fraud_status)
conf
conf$byClass



### Now we build a GLM for all the sites using resampling techniques

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
                   -Medium_Key)

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

set.seed(123)
train <- SMOTE(fraud_status ~., train, perc.over = 1000, perc.under = 150)
table(train$fraud_status)
prop.table(table(train$fraud_status))

# Building a GLM
start <- Sys.time()
model <- glm(fraud_status ~., data = train, family = binomial(link = "logit"))
end <- Sys.time()
time <- end - start
time
summary(model)

# PLotting an ROC curve
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



### Now we build a GLM for all the sites without using resampling techniques

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