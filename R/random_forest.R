library(data.table)
library(randomForest)
library(caret)

setwd("~")
setwd("Documents/university/MSc/fundamental/fraud/")

# Read in Data
site_data <- fread("data/stratified/dataset_119.csv")
fraud <- fread("data/stratified/fraud_data.csv")

# Append DataFrames
data <- rbind(site_data, fraud)

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

# Setup Random Forest
model_rf <- train(fraud_status ~ ., 
                  data = data,
                  preProcess = c("scale", "center"),
                  trControl = trainControl(method = "repeatedcv",
                                           number = 10,
                                           repeats = 3,
                                           verboseIter = TRUE))

final <- data.frame(actual = test_data$fraud_status, 
                    predict(model_rf, newdata = test_data, type = "prob"))
# final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
# cm_original <- confusionMatrix(final$predict, test_data$classes)

# Under Sample now
ctrl <- trainControl(method = "repeatedcv", 
                     number = 2, 
                     repeats = 2,#10 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(123)
nmin <- sum(train_data$fraud_status == 1)
model_rf_under <- train(fraud_status ~ .,
                        data = train_data,
                        method = "rf",
                        preProcess = c("scale", "center"),
                        trControl = ctrl,
                        strata = train_data$fraud_status,
                        sampsize = rep(nmin, 2))

final_under <- data.frame(actual = test_data$fraud_status, 
                          predict(model_rf_under, newdata = test_data, 
                                  type = "prob"))

final_under$predict <- ifelse(final_under$X0 < 0.5, 0, 1)
cm_under <- confusionMatrix(final_under$predict, test_data$fraud_status)
cm_under
# Plot Model
plot(rf_model, ylim = c(0, 0.99))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Variable Importance
var_importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(var_importance), 
                            Importance = round(var_importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_minimal()
plot(cm_under$table)
cm_under$table
