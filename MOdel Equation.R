# Install necessary packages if not already installed
# install.packages(c("corrplot", "dplyr", "car", "caret", "ggplot2", "ggpubr"))
setwd("H:/New folder (2)/A/")
# Load required libraries
library(corrplot)
library(dplyr)
library(car)
library(caret)
library(ggplot2)
library(ggpubr)

# Load dataset (update the file path)
data <- read.csv("selected_data.csv")

# Create the evaluation function: RMSE and R-square
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE / nrow(df))
  data.frame(RMSE = RMSE, Rsquare = R_square)
}

# Step 1: Data Preprocessing
response_var <- colnames(data)[1]

# Compute Pearson correlation matrix
cor_matrix <- cor(data, use = "complete.obs", method = "pearson")

# Select variables highly correlated with response (|r| > 0.3 threshold)
cor_target <- cor_matrix[, response_var]
selected_vars <- names(cor_target[abs(cor_target) > 0.3 & names(cor_target) != response_var])

# Ensure at least 2 predictors remain
if (length(selected_vars) < 2) {
  stop("Error: Too few predictors left after correlation filtering!")
}

# Subset dataset with selected variables
data_selected <- data %>% select(all_of(c(response_var, selected_vars)))

# Step 2: Removing Highly Correlated Predictors (Correlation > 0.85)
cor_matrix_selected <- cor(data_selected[-1], use = "complete.obs", method = "pearson")
highly_correlated <- findCorrelation(cor_matrix_selected, cutoff = 0.85)

if (length(highly_correlated) > 0) {
  cat("Removing highly correlated variables:", colnames(data_selected[-1])[highly_correlated], "\n")
  data_selected <- data_selected %>% select(-highly_correlated)
}

# Step 3: Removing Aliased Coefficients (Perfect Collinearity)
alias_info <- alias(lm(as.formula(paste(response_var, "~ .")), data = data_selected))

if (length(alias_info$Complete) > 0) {
  aliased_vars <- rownames(alias_info$Complete)
  cat("Removing aliased variables due to perfect collinearity:", paste(aliased_vars, collapse = ", "), "\n")
  data_selected <- data_selected %>% select(-all_of(aliased_vars))
}

# Step 4: Checking Multicollinearity Using VIF (Variance Inflation Factor)
calculate_vif <- function(df) {
  model <- lm(as.formula(paste(response_var, "~", paste(colnames(df)[-1], collapse = " + "))), data = df)
  
  # Ensure no aliased coefficients before calculating VIF
  if (any(alias(model)$Complete)) {
    stop("Error: Aliased coefficients found. Adjust predictors before VIF calculation.")
  }
  
  vif_values <- vif(model)
  return(vif_values)
}

# Compute VIF and iteratively remove high VIF variables (>4)
vif_values <- calculate_vif(data_selected)

while (any(vif_values > 4) & ncol(data_selected) > 2) {
  remove_var <- names(which.max(vif_values))
  cat("Removing variable due to high VIF:", remove_var, "\n")
  data_selected <- data_selected %>% select(-all_of(remove_var))
  vif_values <- calculate_vif(data_selected)
}

# Export the cleaned dataset
write.csv(data_selected, "selected_data.csv", row.names = FALSE)

# Step 4: Fit Multiple Linear Regression Model
model <- lm(as.formula(paste(response_var, "~", paste(colnames(data_selected)[-1], collapse = " + "))), data = data_selected)
model_summary <- summary(model)

# Extract Model Coefficients
coefficients <- coef(model)
model_equation <- paste0(response_var, " = ", round(coefficients[1], 3), " + ", 
                         paste(paste0(round(coefficients[-1], 3), " * ", names(coefficients[-1])), collapse = " + "))

# Model Predictions & Evaluation
predictions <- predict(model, newdata = data_selected)
evaluation_results <- eval_results(data_selected[[response_var]], predictions, data_selected)

# Extract Variable Importance (t-statistics)
var_importance <- abs(summary(model)$coefficients[-1, "t value"])
importance_df <- data.frame(Variable = names(var_importance), Importance = var_importance)
top_variables <- names(sort(var_importance, decreasing = TRUE))[1:min(3, length(var_importance))]  # Top 3 variables

# Save Evaluation Data
write.csv(evaluation_results, "model_evaluation.csv", row.names = FALSE)
write.csv(importance_df, "variable_importance_data.csv", row.names = FALSE)

# Step 5: Summary Paragraph
num_samples <- nrow(data_selected)
num_predictors <- ncol(data_selected) - 1  # Exclude response variable
rmse_value <- round(evaluation_results$RMSE, 3)
r2_value <- round(evaluation_results$Rsquare, 3)
top_vars <- paste(top_variables, collapse = ", ")

summary_paragraph <- paste(
  "The dataset was processed to remove highly correlated predictors and multicollinearity issues, ensuring robust regression modeling. ",
  "A total of", num_samples, "samples were used, with", num_predictors, "predictor variables selected after filtering.",
  "\n\nThe multiple linear regression model was fitted to the selected data, resulting in the following equation:\n",
  model_equation,
  "\n\nModel performance evaluation showed an RMSE of", rmse_value, "and an R-squared value of", r2_value, 
  "indicating that the model explains approximately", round(r2_value * 100, 2), "% of the variance in the response variable.",
  "\n\nThe most influential predictors based on t-statistics were:", top_vars,
  "\nThese predictors had the highest contribution to explaining the response variable's variation.",
  "\n\nOverall, this analysis provides insights into the key factors influencing the response variable and offers a solid foundation for predictive modeling."
)

# Print Summary
cat(summary_paragraph)

# Save Summary to File
write(summary_paragraph, file = "Regression_Model_Summary.txt")

# Step 6: Visualization
# Residuals vs Fitted Values
ggplot(data_selected, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") + ylab("Residuals")

# Actual vs Predicted Values
ggplot(data_selected, aes(x = data_selected[[response_var]], y = predictions)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  ggtitle("Actual vs Predicted Values") +
  xlab("Actual Values") + ylab("Predicted Values")

# Save plots
ggsave("Residuals_vs_Fitted.png", width = 6, height = 4, dpi = 300)
ggsave("Actual_vs_Predicted.png", width = 6, height = 4, dpi = 300)

# Final message
cat("\nAnalysis complete. Graphs generated and saved.\n")
