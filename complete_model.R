# Set working directory
setwd("H:/New folder (2)/A/")

# Load necessary libraries
library(prospectr)      # Kennard-Stone algorithm
library(glmnet)         # LASSO, Ridge, and Elastic Net regression
library(car)            # Stepwise regression
library(progress)       # Progress bar
library(doParallel)     # Parallel processing
library(ggplot2)        # Publication-quality plots
library(caret)          # Cross-validation and model training
library(corrplot)       # Correlation matrix visualization
library(tidyr)          # For reshaping data
library(dplyr)          # Data manipulation
library(randomForest)   # Random Forest model
library(e1071)          # SVM for regression
library(gbm)            # Gradient Boosting Machine
library(class)          # K-Nearest Neighbors (KNN)

# Load data
data <- read.csv("selected_data.csv")

# Pearson Correlation Matrix
cor_matrix <- cor(data[, -1], method = "pearson") 
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 1.5, cl.cex = 1.2, title = "Pearson Correlation Matrix", mar = c(0,0,2,0))

# Define evaluation function
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE / nrow(df))
  return(data.frame(RMSE = RMSE, Rsquare = R_square))
}

# Kennard-Stone algorithm for train-test split
xspace <- data[,-1]
ks <- kenStone(as.matrix(xspace), k = min(20, nrow(data) - 5), metric = "mahal", pc = 0.99, .center = TRUE, .scale = FALSE)
trainid <- ks$test

trainingset <- data[trainid, ]
testset <- data[-trainid, ]

x_train <- as.matrix(trainingset[,-1])
y_train <- as.numeric(trainingset[,1])

x_test <- as.matrix(testset[,-1])
y_test <- as.numeric(testset[,1])

# Check dataset size
print(dim(trainingset))
print(dim(testset))

# ---- LINEAR REGRESSION ----
lm_model <- lm(Response ~ ., data = trainingset)
lm_predictions <- predict(lm_model, testset)
lm_fittings <- fitted(lm_model)
lm_results <- eval_results(y_test, lm_predictions, testset)
lm_train_results <- eval_results(y_train, lm_fittings, trainingset)

# ---- LASSO REGRESSION ----
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = min(5, nrow(x_train)))
lasso_predictions <- predict(lasso_model, x_test, s = "lambda.min")
lasso_fittings <- predict(lasso_model, x_train, s = "lambda.min")
lasso_results <- eval_results(y_test, lasso_predictions, testset)
lasso_train_results <- eval_results(y_train, lasso_fittings, trainingset)

# ---- RIDGE REGRESSION ----
ridge_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = min(5, nrow(x_train)))
ridge_predictions <- predict(ridge_model, x_test, s = "lambda.min")
ridge_fittings <- predict(ridge_model, x_train, s = "lambda.min")
ridge_results <- eval_results(y_test, ridge_predictions, testset)
ridge_train_results <- eval_results(y_train, ridge_fittings, trainingset)

# ---- ELASTIC NET ----
elastic_model <- cv.glmnet(x_train, y_train, alpha = 0.5, nfolds = min(5, nrow(x_train)))
elastic_predictions <- predict(elastic_model, x_test, s = "lambda.min")
elastic_fittings <- predict(elastic_model, x_train, s = "lambda.min")
elastic_results <- eval_results(y_test, elastic_predictions, testset)
elastic_train_results <- eval_results(y_train, elastic_fittings, trainingset)

# ---- RANDOM FOREST ----
rf_model <- randomForest(x_train, y_train, ntree = 500, importance = TRUE)
rf_predictions <- predict(rf_model, x_test)
rf_fittings <- predict(rf_model, x_train)
rf_results <- eval_results(y_test, rf_predictions, testset)
rf_train_results <- eval_results(y_train, rf_fittings, trainingset)

# ---- SUPPORT VECTOR MACHINE (SVM) ----
svm_model <- svm(x_train, y_train)
svm_predictions <- predict(svm_model, x_test)
svm_fittings <- predict(svm_model, x_train)
svm_results <- eval_results(y_test, svm_predictions, testset)
svm_train_results <- eval_results(y_train, svm_fittings, trainingset)

# ---- GRADIENT BOOSTING MACHINE (GBM) ----
gbm_model <- gbm(
  Response ~ ., data = trainingset, distribution = "gaussian",
  n.trees = 500, interaction.depth = 5, shrinkage = 0.01, 
  cv.folds = min(3, nrow(x_train)),  
  bag.fraction = 0.5, 
  n.minobsinnode = 2,  
  verbose = FALSE
)
gbm_predictions <- predict(gbm_model, testset, n.trees = 500)
gbm_fittings <- predict(gbm_model, trainingset, n.trees = 500)
gbm_results <- eval_results(y_test, gbm_predictions, testset)
gbm_train_results <- eval_results(y_train, gbm_fittings, trainingset)

# ---- STEPWISE REGRESSION ----
mdl_null <- lm(Response ~ 1, data = trainingset)
mdl_full <- lm(Response ~ ., data = trainingset)
mdl_stepwise <- step(mdl_null, scope = formula(mdl_full), direction = "both", trace = 1, k = log(nrow(trainingset)))
stepwise_predictions <- predict(mdl_stepwise, newdata = testset)
stepwise_results <- eval_results(y_test, stepwise_predictions, testset)
stepwise_train_results <- eval_results(y_train, mdl_stepwise$fitted.values, trainingset)

# ---- MODEL PERFORMANCE TABLE ----
all_results <- data.frame(
  Model = c("Linear Regression", "LASSO", "Ridge", "Elastic Net", "Random Forest", "SVM", "GBM", "Stepwise Regression"),
  Train_R2 = c(lm_train_results$Rsquare, lasso_train_results$Rsquare, ridge_train_results$Rsquare, 
               elastic_train_results$Rsquare, rf_train_results$Rsquare, svm_train_results$Rsquare, 
               gbm_train_results$Rsquare, stepwise_train_results$Rsquare),
  Test_R2 = c(lm_results$Rsquare, lasso_results$Rsquare, ridge_results$Rsquare, 
              elastic_results$Rsquare, rf_results$Rsquare, svm_results$Rsquare, 
              gbm_results$Rsquare, stepwise_results$Rsquare),
  Train_RMSE = c(lm_train_results$RMSE, lasso_train_results$RMSE, ridge_train_results$RMSE, 
                 elastic_train_results$RMSE, rf_train_results$RMSE, svm_train_results$RMSE, 
                 gbm_train_results$RMSE, stepwise_train_results$RMSE),
  Test_RMSE = c(lm_results$RMSE, lasso_results$RMSE, ridge_results$RMSE, 
                elastic_results$RMSE, rf_results$RMSE, svm_results$RMSE, 
                gbm_results$RMSE, stepwise_results$RMSE)
)

write.csv(all_results, "H:/New folder (2)/B/model_comparison_results.csv", row.names = FALSE)
print(all_results)
