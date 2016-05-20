library(lubridate)
library(glmnet)
library(xgboost)
library(ROCR)

setwd("~/00_dsc/repository/kaggle/kaggle_san_francisco_crime_classification")

train <- read.csv("data/train.csv", stringsAsFactors = FALSE)

train$Year <- as.factor(as.numeric(year(train$Dates)))
train$Month <- as.factor(as.numeric(month(train$Dates)))
train$Day <- as.factor(as.numeric(day(train$Dates)))
train$Hour <- as.factor(as.numeric(hour(train$Dates)))
train$WeekDay <- as.factor(as.numeric(wday(train$Dates)))
train$District <- as.factor(as.numeric(as.factor(train$PdDistrict)))
train$Corner <- as.factor(as.numeric(rowSums(as.data.frame(grepl("/", train$Address)))))

cat_names <- levels(as.ordered(train$Category))
target <- as.matrix(as.numeric(as.ordered(train$Category))-1)

train_sub <- subset(train, , -c(Dates, Category, Descript, DayOfWeek, PdDistrict, Resolution, Address))

index <- sample(nrow(train_sub),nrow(train_sub)/10) 
train_sub <- train_sub[index,]
target_sub <- target[index]

rm(train,target)

# Data Preparation

fix_params <- list(booster = "gbtree", 
                   objective = "multi:softprob", 
                   eval_metric = "mlogloss")
set.seed(1967)

# m <- model.matrix( ~ X + Y + District + WeekDay + Year + Month + Day + Hour + Corner, data = df_train)
# model <- xgboost(data = m, label = df_target, param = fix_params, nrounds = 10, num_class=39, verbose = 0)

matrix_train <- xgb.DMatrix(data.matrix(train_sub), label = as.numeric(target_sub))
# model <- xgboost(data = m, param = fix_params, nrounds = 40, num_class=39)
# pred_train <- predict(model, m)
# prob.matrix <- matrix(pred_train, ncol = 39, byrow = T)

cv_done <- FALSE
if (cv_done){
  all_lines <- read.csv("./data/result_cv.csv", header = TRUE, sep = ",")
} else {
  all_lines <- NULL
  for (n in 1:150){
    set.seed(1000+n)
    tree_params <- list(eta = runif(1, 0.010, 0.04),
                        max_depth = sample(5:8, 1),
                        max_delta_step = sample(0:3, 1),
                        subsample = runif(1, 0.7, 0.99),
                        colsample_bytree = runif(1, 0.5, 0.99))
    model_cv <- xgb.cv(param = append(fix_params,tree_params),
                       data = matrix_train,
                       nrounds = 1e4,
                       nfold = 10,
                       early.stop.round = 10,
                       num_class = 39)
    new_line <- data.frame( eta = tree_params$eta,
                            max_depth = tree_params$max_depth,
                            max_delta_step = tree_params$max_delta_step,
                            subsample = tree_params$subsample,
                            colsample_bytree = tree_params$colsample_bytree,
                            best_itr = which.min(model_cv$test.mlogloss.mean),
                            best_mlogloss = min(model_cv$test.mlogloss.mean))
    all_lines <- rbind(all_lines, new_line)
  }
  write.csv(all_lines,"./data/result_cv.csv", row.names = FALSE)
}

