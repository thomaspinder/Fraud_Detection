library(data.table)
library(randomForest)
library(caret)
library(mlbench)
library(pROC)
library(dplyr)

setwd("~")
setwd("Documents/university/MSc/fundamental/fraud/")

# Read in Data
site_data <- fread("data/stratified/dataset_121.csv")
fraud <- fread("data/stratified/fraud_data.csv")

# Append DataFrames
data <- unique(rbind(site_data, fraud))

# Convert Status to Factor
data$fraud_status <- as.factor(data$fraud_status)

# Split into test/train
set.seed(123)
data$fraud_status <- as.factor(data$fraud_status)
data <- data %>% 
  select(-Site_Key)

index <- createDataPartition(data$fraud_status, p = 0.7, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]
table(data$fraud_status)

# RF Using Caret
# Number = Repeats = 10
control <- trainControl(method = "repeatedcv", number = 2, repeats = 1)
metric <- "Accuracy"
mtry <- sqrt(ncol(data))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(fraud_status ~ .,
                    data = train_data,
                    method = "rf",
                    metric = metric,
                    tuneGrid = tunegrid,
                    trControl = control)

to_test <- test_data %>% 
  select(-fraud_status)

prediction <- predict(rf_default, newdata = test_data, type = "prob")
prediction_val <- ifelse(prediction$`0`>0.5, 0, 1)
results <- data.frame(actual = test_data$fraud_status, prediction = prediction_val)
results$result <- ifelse(results$actual==results$prediction, 1, 0)
sum(results$result)/dim(results)[1]


# #Try Original RF
# rf_model <- randomForest(fraud_status ~ . , data = train_data)
# plot(rf_model)
# print(rf_model$confusion)
# legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# 
# # Variable Importance
# var_importance <- importance(rf_model)
# varImportance <- data.frame(Variables = row.names(var_importance), 
#                             Importance = round(var_importance[ ,'MeanDecreaseGini'],2))
# # Plot Variable Rank
# rankImportance <- varImportance %>%
#   mutate(Rank = paste0('#',dense_rank(desc(Importance))))
# ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
#                            y = Importance, fill = Importance)) +
#   geom_bar(stat='identity') + 
#   geom_text(aes(x = Variables, y = 0.5, label = Rank),
#             hjust=0, vjust=0.55, size = 4, colour = 'red') +
#   labs(x = 'Variables') +
#   coord_flip() + 
#   theme_minimal()
# 
