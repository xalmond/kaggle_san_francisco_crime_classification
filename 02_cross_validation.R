library(glmnet)
library(xgboost)

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

# Split dataframe, target = crime categories (0:38)

cat_names <- levels(as.ordered(train$Category))
target <- as.matrix(as.numeric(as.ordered(train$Category))-1)
train$Category <- NULL

# XGBoost data Preparation

matrix_train <- xgb.DMatrix(data.matrix(train), label = as.numeric(target))

fix_params <- list(booster = "gbtree", 
                   objective = "multi:softprob", 
                   eval_metric = "mlogloss")

# Selecting best booster parameters

param_done <- TRUE
if (param_done){
  best_param <- read.csv("./data/result_param.csv", header = TRUE, sep = ",")
} else {
  best_param <- NULL
  for (n in 0:150){
    set.seed(1000+n)
    tree_params <- list(eta = runif(1, 0.010, 0.04),
                        max_depth = sample(5:8, 1),
                        max_delta_step = sample(0:3, 1),
                        subsample = runif(1, 0.7, 0.99),
                        colsample_bytree = runif(1, 0.5, 0.99))
    model_cv <- xgb.cv(param = append(fix_params,tree_params),
                       data = matrix_train,
                       nrounds = 5,
                       nfold = 10,
                       early.stop.round = 10,
                       num_class = 39,
                       verbose = 0)
    new_line <- data.frame(eta = tree_params$eta,
                           max_depth = tree_params$max_depth,
                           max_delta_step = tree_params$max_delta_step,
                           subsample = tree_params$subsample,
                           colsample_bytree = tree_params$colsample_bytree,
                           best_itr = which.min(model_cv$test.mlogloss.mean),
                           best_mlogloss = min(model_cv$test.mlogloss.mean))
    best_param <- rbind(best_param, new_line)
  }
  best_param <- best_param[order(best_param$best_mlogloss),]
  write.csv(best_param,"./data/result_param.csv", row.names = FALSE)
}

best_param[1,1:5]

# Selecting best number of iteractions

cv_done <- TRUE
if (cv_done){
  best <- read.csv("./data/result_cv.csv", header = TRUE)
} else {
  tree_params <- list(eta = param[1,1],
                      max_depth = param[1,2],
                      max_delta_step = param[1,3],
                      subsample = param[1,4],
                      colsample_bytree = param[1,5])
  model_cv <- xgb.cv(param = append(fix_params,tree_params),
                     data = matrix_train,
                     nrounds = 1e4,
                     nfold = 10,
                     early.stop.round = 10,
                     num_class = 39)
  best <- data.frame(eta = tree_params$eta,
                     max_depth = tree_params$max_depth,
                     max_delta_step = tree_params$max_delta_step,
                     subsample = tree_params$subsample,
                     colsample_bytree = tree_params$colsample_bytree,
                     best_itr = which.min(model_cv$test.mlogloss.mean),
                     best_mlogloss = min(model_cv$test.mlogloss.mean))
  write.csv(best,"./data/result_cv.csv", row.names = FALSE)
}

best[,6:7]
