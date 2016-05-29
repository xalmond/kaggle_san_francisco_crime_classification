library(lubridate)
library(glmnet)
library(xgboost)
library(Ckmeans.1d.dp)
library(ggplot2)
library(data.table)

# Reading featured training dataset

train <- read.csv("data/train_eng.csv", stringsAsFactors = FALSE)

train$District <- as.factor(train$District)
train$Corner <- as.factor(train$Corner)
train$Year <- as.factor(train$Year)
train$Month <- as.factor(train$Month)
train$Day <- as.factor(train$Day)
train$Hour <- as.factor(train$Hour)
train$WeekDay <- as.factor(train$WeekDay)
train$Night <- as.factor(train$Night)
train$Holiday <- as.factor(train$Holiday)

# Split dataframe, target = crime categories

cat_names <- levels(as.ordered(train$Category))
target <- as.matrix(as.numeric(as.ordered(train$Category))-1)
train$Category <- NULL

# XGBoost model training

model_done <- TRUE
if (model_done){
  model <- xgb.load("./data/xgb.model")
  imp_matrix <- setDT(read.csv("data/result_imp.csv", header = TRUE))
} else {
  matrix_train <- xgb.DMatrix(data.matrix(train), label = as.numeric(target))
  set.seed(1967)
  fix_params <- list(booster = "gbtree", 
                     objective = "multi:softprob", 
                     eval_metric = "mlogloss")
  best <- read.csv("./data/result_cv.csv", header = TRUE, sep = ",")
  tree_params <- list(eta = best[1,1],
                      max_depth = best[1,2],
                      max_delta_step = best[1,3],
                      subsample = best[1,4],
                      colsample_bytree = best[1,5])
  model <- xgboost(data = matrix_train, param = append(fix_params,tree_params), nrounds = best[1,6], num_class=39)
  xgb.save(model,"./data/xgb.model")  

  imp_matrix <- xgb.importance(feature_names = names(train), model = model)
  write.csv(imp_matrix,"./data/result_imp.csv", row.names = FALSE)
}

# Importance of variables

imp_matrix
xgb.plot.importance(imp_matrix, numberOfClusters = 3)

# Prepare submission

test <- read.csv("data/test_eng.csv", stringsAsFactors = FALSE)

test$District <- as.factor(test$District)
test$Corner <- as.factor(test$Corner)
test$Year <- as.factor(test$Year)
test$Month <- as.factor(test$Month)
test$Day <- as.factor(test$Day)
test$Hour <- as.factor(test$Hour)
test$WeekDay <- as.factor(test$WeekDay)
test$Night <- as.factor(test$Night)
test$Holiday <- as.factor(test$Holiday)

# Split dataframe Id

test_id <- test$Id
test$Id <- NULL

# Making test prediction and submission file

pred_test <- predict(model, data.matrix(test))
prob_matrix <- matrix(pred_test, ncol = 39, byrow = T)

submission <- as.data.frame(prob_matrix)
colnames(submission) <- cat_names
submission <- cbind(test_id,submission)
colnames(submission)[1] <- "Id"

write.csv(submission, "./submission/submission.csv", row.names = FALSE)
