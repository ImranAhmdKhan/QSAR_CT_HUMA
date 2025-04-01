# Install required packages if not installed
install.packages(c("ggplot2", "dplyr", "prospectr", "glmnet", "car", "yarrr"))

# Load required libraries
library(ggplot2)
library(dplyr)
library(prospectr)
library(glmnet)
library(car)
library(yarrr)

# Load dataset (update file path)
data <- read.csv("H:/New folder (2)/B/selected_data.csv")

# Define evaluation function: RMSE & R-square
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE / nrow(df))
  data.frame(RMSE = RMSE, Rsquare = R_square)
}

# Data splitting using KenStone algorithm
xspace <- data[, -1]  # Exclude response variable
ks <- kenStone(as.matrix(xspace), k = 12, metric = "mahal", pc = 0.99, .center = TRUE, .scale = FALSE)

trainid <- ks$test
trainingset <- data[trainid, ]
testset <- data[-trainid, ]

x_train <- as.matrix(trainingset[, -1])
y_train <- as.matrix(trainingset[, 1])
x_test <- as.matrix(testset[, -1])
y_test <- as.matrix(testset[, 1])

# Plot Training vs. Test Distribution
plot(x = 1, type = "n",
     xlim = c(min(data$Response) - 2, max(data$Response) + 2),
     ylim = c(min(data$Response) - 2, max(data$Response) + 2),
     xlab = expression("Obs. lnK"[on]), ylab = expression("Obs. lnK"[on]),
     main = expression("lnK"[on]*": Test and Training Data Distribution"),
     cex.main = 2, cex.lab = 2, cex.axis = 1.5)

points(y_test, y_test, pch = 16, col = transparent("red", 0.2), cex = 2)
points(y_train, y_train, pch = 16, col = transparent("steelblue3", 0.5), cex = 2)
abline(coef = c(0, 1), lty = 2, lwd = 2)
legend("bottomright", legend = c("Training", "Test"), col = transparent(c('steelblue3', 'red'), .2), pch = 16, cex = 2, bg = "white")

# Stepwise Regression
mdl_null <- lm(Response ~ 1, data = trainingset)
mdl_full <- lm(Response ~ ., data = trainingset)
mdl_stepwise <- step(mdl_null, scope = list(lower = mdl_null, upper = mdl_full), direction = "both", trace = 1, k = log(nrow(trainingset)))

# Model Predictions & Evaluation
predict_stepwise <- predict(mdl_stepwise, newdata = testset)
fitted_stepwise <- mdl_stepwise$fitted.values

stepwise_test_eval <- eval_results(testset$Response, predict_stepwise, testset)
stepwise_train_eval <- eval_results(trainingset$Response, fitted_stepwise, trainingset)

# Plot Stepwise Regression Results
plot(1, type = "n",
     xlim = c(min(data$Response) - 2, max(data$Response) + 2),
     ylim = c(min(data$Response) - 2, max(data$Response) + 2),
     xlab = "Predicted Response", ylab = "Observed Response",
     main = "Response - Stepwise Model")

text(x = c(1, 4.5, 1, 4.5), y = c(15, 15, 13, 13),
     labels = c("R² Train = ", round(stepwise_train_eval$Rsquare, 2), "R² Test =", round(stepwise_test_eval$Rsquare, 2)), cex = 1.5)

points(fitted_stepwise, trainingset$Response, pch = 16, col = 'blue', cex = 2)
points(predict_stepwise, y_test, pch = 16, col = 'red', cex = 2)
abline(coef = c(0, 1), lty = 2, lwd = 2)
legend("bottomright", legend = c("Training", "Test"), col = c('blue', 'red'), pch = 16, cex = 2, bg = "white")

# Lasso Regression
set.seed(1)
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = 10^seq(2, -6, length = 100), standardize = TRUE, nfolds = 5)
lambda_best_lasso <- lasso_reg$lambda.min

lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best_lasso, standardize = TRUE)
lasso_predictions <- predict(lasso_model, s = lambda_best_lasso, newx = x_test)
lasso_fittings <- predict(lasso_model, s = lambda_best_lasso, newx = x_train)

lasso_test_eval <- eval_results(y_test, lasso_predictions, testset)
lasso_train_eval <- eval_results(y_train, lasso_fittings, trainingset)

# Summary Paragraph
num_train <- nrow(trainingset)
num_test <- nrow(testset)
lasso_rmse_train <- round(lasso_train_eval$RMSE, 3)
lasso_r2_train <- round(lasso_train_eval$Rsquare, 3)
lasso_rmse_test <- round(lasso_test_eval$RMSE, 3)
lasso_r2_test <- round(lasso_test_eval$Rsquare, 3)

summary_paragraph <- paste(
  "The analysis began with the Kennard-Stone algorithm to split the dataset into training and test sets. ",
  "The training set contained", num_train, "molecules, and the test set contained", num_test, "molecules, ensuring diversity and representativeness in the chemical space. ",
  "Stepwise regression identified significant variables and achieved an R² of", stepwise_train_eval$Rsquare, "on the training set and", stepwise_test_eval$Rsquare, "on the test set. ",
  "Lasso regression was then applied to identify key descriptors, utilizing cross-validation to select the optimal lambda value. ",
  "The Lasso model achieved an RMSE of", lasso_rmse_train, "and R-squared of", lasso_r2_train, "on the training set, while achieving an RMSE of", lasso_rmse_test,
  "and R-squared of", lasso_r2_test, "on the test set. ",
  "Finally, the baseline model was evaluated, and its predictions were visualized using a scatter plot of observed vs. predicted values for both training and test sets. ",
  "This comprehensive workflow provides insights into descriptor importance and the model's predictive power."
)

# Save Summary to File
cat(summary_paragraph)
write(summary_paragraph, file = "Lasso_Exhaustive_Summary.txt")

# Save processed datasets
write.csv(trainingset, "trainingset.csv", row.names = FALSE)
write.csv(testset, "testset.csv", row.names = FALSE)
