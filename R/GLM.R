rm(list = ls())
library(caret)
setwd("C:/Users/Luke/Documents/University/Lancaster/Data Fundamentals/thgfd/data")
data <- read.csv("LUKE_FINAL_GLM_DATA.csv")

data <- with(data, data.frame(model.matrix(~Category_Level_2 -1), data))

# data$prop <- as.numeric(data$prop)
# data$canc_prop <- as.numeric(data$canc_prop)
# data$Product_Charge_Price <- as.numeric(data$Product_Charge_Price)
# 
# data$fraud_status <- as.factor(data$fraud_status)

# for (i in 1:40){
#   data[,i] <- as.factor(data[,i])
# }
# for (i in 42:54){
#   data[,i] <- as.factor(data[,i])
# }
# for (i in 56){
#   data[,i] <- as.factor(data[,i])
# }

#' set.seed(123)
#' index <- createDataPartition(data$fraud_status, p = 0.7, list = FALSE)
#' train <- data[index, ]
#' test <- data[-index, ]
#' 
#' #' Temporarilty make train much smaller
#' set.seed(123)
#' train_minimal <- train[sample(1:nrow(train), size = 2000, replace = FALSE),]

#' #' Building the GLM
#' start <- Sys.time()
#' GLM <- glm(fraud_status ~., data = train, family = binomial(link = "logit"))
#' end <- Sys.time()
#' time <- end - start
#' time

#' #' Testing the GLM
#' p <- predict(GLM, train)
#' p
#' tab <- table(p, train$fraud_status)
#' tab
