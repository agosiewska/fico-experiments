load("data/01_GS_prepro_tv_imputation.Robj")

library(h2o)

h2o.init()

train$RiskPerformance <- factor(train$RiskPerformance, levels = c("Good", "Bad"))
test$RiskPerformance <- factor(test$RiskPerformance, levels = c("Good", "Bad"))

train_h2o <- as.h2o(train)
test_h2o <- as.h2o(test)

# Identify predictors and response
y <- "RiskPerformance"
x <- setdiff(names(train_h2o), y)

# For binary classification, response should be a factor
train_h2o[,y] <- as.factor(train_h2o[,y])
test_h2o[,y] <- as.factor(test_h2o[,y])


######################################################

# Run AutoML for 20 base models (limited to 1 hour max runtime)
aml <- h2o.automl(x = x, y = y,
                  training_frame = train_h2o,
                  max_models = 10000,
                  max_runtime_secs = 1 * 3600,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
model <- aml@leader

# h2o.saveModel(aml@leader, path = ".")
# model <- h2o.loadModel(".\\models\\StackedEnsemble_AllModels_AutoML_20190523_201951")

pred_test <- as.data.frame(h2o.predict(model, test_h2o))
pred_test <- pred_test[,3]
pred_train <- as.data.frame(h2o.predict(model, train_h2o))
pred_train <- pred_train[,3]

# How good is this model?
mltools::auc_roc(pred_test, test$RiskPerformance == "Good")
mltools::auc_roc(pred_train, train$RiskPerformance == "Good")
 


######################################################

# Run AutoML for 20 base models (limited to 2 hours max runtime)
aml <- h2o.automl(x = x, y = y,
                  training_frame = train_h2o,
                  max_models = 10000,
                  max_runtime_secs = 2 * 3600,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
model <- aml@leader

# h2o.saveModel(aml@leader, path = ".")
# model <- h2o.loadModel(".\\models\\StackedEnsemble_AllModels_AutoML_20190523_201951")

pred_test <- as.data.frame(h2o.predict(model, test_h2o))
pred_test <- pred_test[,3]
pred_train <- as.data.frame(h2o.predict(model, train_h2o))
pred_train <- pred_train[,3]

# How good is this model?
mltools::auc_roc(pred_test, test$RiskPerformance == "Good")
mltools::auc_roc(pred_train, train$RiskPerformance == "Good")



######################################################

# Run AutoML for 20 base models (limited to 8 hours max runtime)
aml <- h2o.automl(x = x, y = y,
                  training_frame = train_h2o,
                  max_models = 10000,
                  max_runtime_secs = 8 * 3600,
                  seed = 1)

# View the AutoML Leaderboard
lb <- aml@leaderboard
model <- aml@leader

# h2o.saveModel(aml@leader, path = ".")
# model <- h2o.loadModel(".\\models\\StackedEnsemble_AllModels_AutoML_20190523_201951")

pred_test <- as.data.frame(h2o.predict(model, test_h2o))
pred_test <- pred_test[,3]
pred_train <- as.data.frame(h2o.predict(model, train_h2o))
pred_train <- pred_train[,3]

# How good is this model?
mltools::auc_roc(pred_test, test$RiskPerformance == "Good")
mltools::auc_roc(pred_train, train$RiskPerformance == "Good")


