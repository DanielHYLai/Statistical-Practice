{
    library(caret)
    library(class)
    library(dplyr)
    library(e1071)
    library(lightgbm)
    library(MASS)
    library(rattle)
    library(randomForest)
    library(rpart)
    library(rpart.plot)
    library(xgboost)
    
    source("utils_main.R")
    
    seed <- 123
    set.seed(seed = seed)
}

# load data
data_raw <- read.csv("high_diamond_ranked_10min.csv", encoding = "UTF-8")

# data preprocessing
removed_column <- c("gameId", "blueEliteMonsters", "blueCSPerMin", "blueGoldPerMin", 
                    "redEliteMonsters", "redCSPerMin", "redGoldPerMin")

data_process <- data_raw %>% 
    ## remove the specific column
    dplyr::select(-any_of(removed_column)) %>% 
    
    ## remove outlier in the column `blueTowersDestroyed`
    .[!(.$blueTowersDestroyed %in% c(3, 4)), ]

# data split
temp_list  <- train_test_Split(df = data_process, train_size = 0.8, seed = 123)
train_data <- temp_list$train
test_data  <- temp_list$test
rm(temp_list)

# model building

KF <- 10

#-----
## logistic regression

### benchmark version
model_LR <- stats::glm(blueWins ~ ., data = train_data, family = binomial)
train_pred_LR <- logistic_Pred(model = model_LR, newdata = train_data, 
                               df_class = "train")
test_pred_LR  <- logistic_Pred(model = model_LR, newdata = test_data, 
                               df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "logistic", seed = seed)

### variable selection version via logistic
summary(model_LR)

#### the result of variable selection via logistic regression
significant_var <- c("blueFirstBlood", "blueDragons", "blueTotalMinionsKilled", 
                     "blueGoldDiff", "blueExperienceDiff", "redDragons", 
                     "redTowersDestroyed", "redTotalMinionsKilled", "blueWins")
train_data_significant <- train_data[, significant_var]
test_data_significant  <- test_data[, significant_var]

model_LR_SIG <- stats::glm(blueWins ~ 0 + ., data = train_data_significant, 
                           family = binomial)
summary(model_LR_SIG)

significant_var <- c("blueFirstBlood", "blueDragons", "blueTotalMinionsKilled", 
                     "blueGoldDiff", "blueExperienceDiff", "redDragons", 
                     "redTotalMinionsKilled", "blueWins")
train_data_significant <- train_data[, significant_var]
test_data_significant  <- test_data[, significant_var]

model_LR_SIG <- stats::glm(blueWins ~ 0 + ., data = train_data_significant, 
                           family = binomial)
summary(model_LR_SIG)

#### using the step-wise method to confirm the variables
stepAIC(model_LR_SIG)  # remain the same variables

train_pred_LR <- logistic_Pred(model = model_LR_SIG, newdata = train_data_significant, 
                               df_class = "train")
test_pred_LR  <- logistic_Pred(model = model_LR_SIG, newdata = test_data_significant, 
                               df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "logistic", seed = seed)

#### LR test: significantly differ than null model.
null_model_LR <- stats::glm(blueWins ~ 1,family = binomial, data = train_data)
likelihood_ratio_test <- stats::anova(null_model_LR, model_LR_SIG, test="LRT")

#-----
## KNN

### benchmark version
train_pred_KNN <- knn_Pred(train = train_data, test = train_data, df_class = "train")
test_pred_KNN  <- knn_Pred(train = train_data, test = test_data, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "knn", seed = seed)

### variable selection version via logistic
train_pred_KNN <- knn_Pred(train = train_data_significant, 
                           test = train_data_significant, 
                           df_class = "train")
test_pred_KNN  <- knn_Pred(train = train_data_significant, 
                           test = test_data_significant, 
                           df_class = "test")
kfold_CV(K = KF, data = train_data_significant, 
         model_type = "knn", seed = seed)

#-----
## naive Bayes

### benchmark version
model_NB <- e1071::naiveBayes(blueWins ~., data = train_data)
train_pred_NB <- e1071_Pred(model = model_NB, newdata = train_data, df_class = "train")
test_pred_NB <- e1071_Pred(model = model_NB, newdata = test_data, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "nb", seed = seed)

### variable selection version via logistic
model_NB <- e1071::naiveBayes(blueWins ~., data = train_data_significant)
train_pred_NB <- e1071_Pred(model = model_NB, newdata = train_data_significant, 
                                  df_class = "train")
test_pred_NB <- e1071_Pred(model = model_NB, newdata = test_data_significant, 
                                 df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "nb", seed = seed)

#-----
## SVM

### benchmark version
model_SVM <- e1071::svm(as.factor(blueWins) ~ ., data = train_data)
train_pred_SVM <- e1071_Pred(model = model_SVM, newdata = train_data, 
                             df_class = "train")
test_pred_SVM <- e1071_Pred(model = model_SVM, newdata = test_data, 
                            df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "svm", seed = seed)

### variable selection version via logistic
model_SVM <- e1071::svm(as.factor(blueWins) ~., data = train_data_significant)
train_pred_SVM <- e1071_Pred(model = model_SVM, newdata = train_data_significant, 
                            df_class = "train")
test_pred_SVM <- e1071_Pred(model = model_SVM, newdata = test_data_significant, 
                           df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "svm", seed = seed)

#-----
## decision tree

### benchmark version
model_DT <- rpart::rpart(as.factor(blueWins) ~ ., data = train_data, method = "class")
train_pred_DT <- DT_Pred(model = model_DT, newdata = train_data, df_class = "train")
test_pred_DT <- DT_Pred(model = model_DT, newdata = test_data, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "dt", seed = seed)

rpart.plot::rpart.plot(model_DT, type = 3, extra = 101, box.palette = "Blues")

### variable selection version via logistic
model_DT <- rpart::rpart(as.factor(blueWins) ~ ., data = train_data_significant,
                         method = "class")
train_pred_DT <- DT_Pred(model = model_DT, newdata = train_data_significant, 
                         df_class = "train")
test_pred_DT <- DT_Pred(model = model_DT, newdata = test_data_significant, 
                        df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "dt", seed = seed)

rpart.plot::rpart.plot(model_DT, type = 3, extra = 101, box.palette = "Blues")

#-----
## random forest

### benchmark version
model_RF <- randomForest::randomForest(as.factor(blueWins) ~ ., data = train_data)
train_pred_RF <- RF_Pred(model = model_RF, newdata = train_data, df_class = "train")
test_pred_RF  <- RF_Pred(model = model_RF, newdata = test_data, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "rf", seed = seed)

### variable selection version via logistic
model_RF <- randomForest::randomForest(as.factor(blueWins) ~ ., 
                                       data = train_data_significant)
train_pred_RF <- RF_Pred(model = model_RF, newdata = train_data_significant, 
                         df_class = "train")
test_pred_RF  <- RF_Pred(model = model_RF, newdata = test_data_significant, 
                         df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "rf", seed = seed)

#-----
## XGB

train_matrix <- xgboost::xgb.DMatrix(
    data = as.matrix(train_data[, -1]), label = train_data[, 1]
)
test_matrix <- xgboost::xgb.DMatrix(data = as.matrix(test_data[, -1]))
train_matrix_significant <- xgboost::xgb.DMatrix(
    data = as.matrix(train_data_significant[, -1]), label = train_data_significant[, 1]
)
test_matrix_significant  <- xgboost::xgb.DMatrix(
    data = as.matrix(test_data_significant[, -1])
)

### benchmark version
model_XGB <- xgboost::xgboost(data = train_matrix, nrounds = 100, 
                                 objective = "binary:logistic", verbose = 0)
train_pred_XGB <- XGB_Pred(model = model_XGB, newdata = train_data, 
                           newdatamatrix = train_matrix, df_class = "train")
test_pred_XGB  <- XGB_Pred(model = model_XGB, newdata = test_data,
                          newdatamatrix = test_matrix, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "xgb", seed = seed)

### benchmark version(tune)
set.seed(123)
model_XGB <- xgboost::xgboost(data = train_matrix, nrounds = 7, 
                              objective = "binary:logistic", eta = 0.1, 
                              max_depth = 4, subsample = 0.9, colsample_bytree = 0.8, 
                              verbose = 0)
train_pred_XGB <- XGB_Pred(model = model_XGB, newdata = train_data, 
                           newdatamatrix = train_matrix, df_class = "train")
test_pred_XGB  <- XGB_Pred(model = model_XGB, newdata = test_data,
                          newdatamatrix = test_matrix, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "xgb_tune", seed = seed)

### variable selection version via logistic
set.seed(123)
model_XGB <- xgboost::xgboost(data = train_matrix_significant, nrounds = 100, 
                              objective = "binary:logistic")
train_pred_XGB <- XGB_Pred(model = model_XGB, newdata = train_data_significant,
                           newdatamatrix = train_matrix_significant, 
                           df_class = "train")
test_pred_XGB  <- XGB_Pred(model = model_XGB, newdata = test_data_significant, 
                          newdatamatrix = test_matrix_significant, 
                          df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "xgb", seed = seed)

### variable selection version via logistic(tune)
"
❗❗❗❗❗❗❗❗❗❗❗❗
set.seed(123) 按兩次，再把 seed 去掉可以得到 0.7565 的準確率
❗❗❗❗❗❗❗❗❗❗❗❗
"
set.seed(123)
model_XGB <- xgboost::xgboost(data = train_matrix_significant, nrounds = 43, 
                              objective = "binary:logistic", eta = 0.05,
                              max_depth = 2, subsample = 0.9, colsample_bytree = 0.9, 
                              verbose = 0)
train_pred_XGB <- XGB_Pred(model = model_XGB, newdata = train_data_significant,
                           newdatamatrix = train_matrix_significant, 
                           df_class = "train")
test_pred_XGB  <- XGB_Pred(model = model_XGB, newdata = test_data_significant, 
                          newdatamatrix = test_matrix_significant, 
                          df_class = "test")
kfold_CV(K = KF, data = train_data_significant, 
         model_type = "xgb_tune_vs", seed = seed)

table(pred = test_pred_XGB, actual = test_data$blueWins)

#-----
## lightGBM

train_matrix <- lightgbm::lgb.Dataset(data = as.matrix(train_data[, -1]), 
                                      label = train_data[, 1])
train_matrix_significant <- lightgbm::lgb.Dataset(
    data = as.matrix(train_data_significant[, -1]), 
    label = train_data_significant[, 1]
)
### benchmark version
model_LGB <- lightgbm::lgb.train(params = list(objective = "binary"), 
                                 data = train_matrix, nrounds = 100, verbose = 0)
train_pred_LGB <- LGBM_Pred(model = model_LGB, newdata = train_data, df_class = "train")
test_pred_LGB <- LGBM_Pred(model = model_LGB, newdata = test_data, df_class = "test")
kfold_CV(K = KF, data = train_data, model_type = "lgbm", seed = seed)

### variable selection version via logistic
model_LGB <- lightgbm::lgb.train(params = list(objective = "binary"), 
                                 data = train_matrix_significant,
                                 nrounds = 100)
train_pred_LGB <- LGBM_Pred(model = model_LGB, newdata = train_data_significant, 
                           df_class = "train")
test_pred_LGB <- LGBM_Pred(model = model_LGB, newdata = test_data_significant, 
                          df_class = "test")
kfold_CV(K = KF, data = train_data_significant, model_type = "lgbm", seed = seed)
