{
    source("requirements.R")
}

train_test_Split <- function(df, train_size, seed = NULL) {
    if (is.null(seed) == FALSE) {
        set.seed(seed = seed)
    }
    
    result <- df[sample(nrow(df)), ]
    
    train_index <- 1:floor(train_size * nrow(result))
    
    train_data <- result[train_index, ]
    test_data  <- result[-train_index, ]
    
    return(list(train = train_data, test = test_data))
}

accuracy_Calculate <- function(pred, actual, df_class, display = TRUE) {
  confusion_matrix <- table(Predicted = pred, Actual = actual)
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  precision <- confusion_matrix[2, 2] / 
    (confusion_matrix[2, 2] + confusion_matrix[2, 1])
  
  recall <- confusion_matrix[2, 2] / 
    (confusion_matrix[2, 2] + confusion_matrix[1, 2])
  
  if (display == TRUE) {
      print(paste("Accuracy  (", df_class, "):", round(accuracy, 4)))
      print(paste("Precision (", df_class, "):", round(precision, 4)))
      print(paste("Recall    (", df_class, "):", round(recall, 4)))
      
      return(c(accuracy, precision, recall))
  }
  else {
      return(c(accuracy, precision, recall))
  }
}

kfold_CV <- function(K, data, model_type = NULL, seed = NULL) {
    
    if (is.null(seed) == FALSE) {
        set.seed(seed = seed)
    }
    
    n <- nrow(data)
    folds <- sample(rep(1:K, length.out = n))
    accuracy_result  <- c()
    precision_result <- c()
    recall_result    <- c()
    
    for (k in c(1:K)) {
        train_data              <- data[folds != k, ]
        validation_data         <- data[folds == k, ]
        train_data_matrix       <- xgboost::xgb.DMatrix(
          data = as.matrix(train_data[, -1]), label = train_data[, 1])
        validation_data_matrix  <- xgboost::xgb.DMatrix(
          data = as.matrix(validation_data[, -1]), label = validation_data[, 1])
        train_data_matrix2      <- lightgbm::lgb.Dataset(
            data = as.matrix(train_data[, -1]), 
            label = train_data[, 1]
        )
        validation_data_matrix2 <- lightgbm::lgb.Dataset(
            data = as.matrix(validation_data[, -1]), 
            label = validation_data[, 1]
        )
        
        if (tolower(model_type) == "logistic") {
            model <- stats::glm(blueWins ~ ., data = train_data, family = binomial)
            pred  <- stats::predict(model, newdata = validation_data, 
                                    type = "response")
            pred  <- ifelse(pred > 0.5, 1, 0)
        }
        else if (tolower(model_type) == "knn") {
            pred <- class::knn(train = train_data[, -1], test = validation_data[, -1], 
                               cl = train_data[, 1], k = 2)
        }
        else if (tolower(model_type) == "nb") {
            model <- e1071::naiveBayes(blueWins ~., data = train_data)
            pred  <- stats::predict(model, newdata = validation_data)
        }
        else if (tolower(model_type) == "svm") {
            model <- e1071::svm(as.factor(blueWins) ~ ., data = train_data)
            pred  <- stats::predict(model, newdata = validation_data)
        }
        else if (tolower(model_type) == "dt") {
            model <- rpart::rpart(as.factor(blueWins) ~ ., data = train_data, 
                                  method = "class")
            pred  <- stats::predict(model, newdata = validation_data, 
                                    type = "class")
        }
        else if (tolower(model_type) == "rf") {
            model <- randomForest::randomForest(as.factor(blueWins) ~ ., 
                                                data = train_data)
            pred  <- stats::predict(model, newdata = validation_data)
        }
        else if (tolower(model_type) == "xgb") {
            model <- xgboost::xgboost(data = train_data_matrix, nrounds = 100, 
                                        objective = "binary:logistic", verbose = 0)
            pred  <- stats::predict(model, newdata = validation_data_matrix)
            pred  <- ifelse(pred > 0.5, 1, 0)
            
        }
        else if (tolower(model_type) == "xgb_tune") {
            model <- xgboost::xgboost(data = train_data_matrix, nrounds = 7, 
                                      objective = "binary:logistic", eta=0.1,
                                      max_depth=4, subsample=0.9, colsample_bytree=0.8, 
                                      verbose = 0)
            pred  <- stats::predict(model, newdata = validation_data_matrix)
            pred  <- ifelse(pred > 0.5, 1, 0)
        }
        else if (tolower(model_type) == "xgb_tune_vs") {
            model <- xgboost::xgboost(data = train_data_matrix, nrounds = 43, 
                                      objective = "binary:logistic", eta = 0.05, 
                                      max_depth = 2, subsample = 0.9, 
                                      colsample_bytree = 0.9, verbose = 0)
            pred  <- stats::predict(model, newdata = validation_data_matrix)
            pred  <- ifelse(pred > 0.5, 1, 0)
        }
        else if (tolower(model_type) == "lgbm") {
            model <- lightgbm::lgb.train(params = list(objective = "binary"),
                                         data = train_data_matrix2, 
                                         nrounds = 100, verbose = 0)
            pred  <- stats::predict(model, newdata = as.matrix(validation_data[, -1]))
            pred  <- ifelse(pred > 0.5, 1, 0)
        }
        
        accuracy_result[k]  <- accuracy_Calculate(
            pred = pred, actual = validation_data$blueWins, df_class = "val", 
            display = FALSE
        )[1]
        precision_result[k] <- accuracy_Calculate(
            pred = pred, actual = validation_data$blueWins, df_class = "val", 
            display = FALSE
        )[2]
        recall_result[k]    <- accuracy_Calculate(
            pred = pred, actual = validation_data$blueWins, df_class = "val", 
            display = FALSE
        )[3]
    }
    print(paste("Accuracy  (K-fold):", round(mean(accuracy_result), 4)))
    print(paste("Precision (K-fold):", round(mean(precision_result), 4)))
    print(paste("Recall    (K-fold):", round(mean(recall_result), 4)))
}

logistic_Pred <- function(model, newdata, df_class) {
    pred <- stats::predict(model, newdata, type = "response")
    pred <- ifelse(pred > 0.5, 1, 0)
    accuracy_Calculate(pred = pred, actual = newdata$blueWins, df_class = df_class)
    
    return(pred)
}

knn_Pred <- function(train, test, df_class) {
    pred <- class::knn(train = train[, -1], test = test[, -1], 
                       cl = train_data[, 1], k = 2)
    accuracy_Calculate(pred = pred, actual = test$blueWins, df_class = df_class)
    
    return(pred)
}

e1071_Pred <- function(model, newdata, df_class) {
    pred <- stats::predict(model, newdata)
    accuracy_Calculate(pred = pred, actual = newdata$blueWins, df_class = df_class)
    
    return(pred)
}

DT_Pred <- function(model, newdata, df_class) {
    pred <- stats::predict(model, newdata, type = "class")
    accuracy_Calculate(pred = pred, actual = newdata$blueWins, df_class = df_class)
    
    return(pred)
}

RF_Pred <- function(model, newdata, df_class) {
    if (df_class == "train") {
        pred <- model[["predicted"]]
    }
    else if (df_class == "test") {
        pred <- stats::predict(model, newdata, type = "class")
    }
    accuracy_Calculate(pred = pred, actual = newdata$blueWins, df_class = df_class)
  
    return(pred)
}

XGB_Pred <- function(model, newdata, newdatamatrix, df_class) {
    pred <- stats::predict(model, newdatamatrix)
    pred <- ifelse(pred > 0.5, 1, 0)
    accuracy_Calculate(pred = pred, actual = newdata$blueWins, df_class = df_class)
  
    return(pred)
}

LGBM_Pred <- function(model, newdata, df_class) {
    pred <- stats::predict(model, as.matrix(newdata[, -1]))
    pred <- ifelse(pred > 0.5, 1, 0)
    accuracy_Calculate(pred = pred, actual = newdata$blueWins, df_class = df_class)
  
    return(pred)
} 