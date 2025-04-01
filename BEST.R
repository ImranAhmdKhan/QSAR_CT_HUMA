# Install necessary packages if not already installed
# install.packages(c("corrplot", "dplyr", "car", "caret", "ggplot2", "ggpubr"))

# Load required libraries
library(corrplot)

library(dplyr)
library(car)
library(caret)
library(ggplot2)
library(ggpubr)

# Load dataset (update the file path)
setwd("H:/New folder (2)/A/")# Load required libraries
data <- read.csv("A.csv")
#data <- read.csv("G:/ABRAR/New Folder9/sugar/input/New folder (2)/SI/selected_data.csv")

# Create the evaluation function: eval_results, which contains RMSE and Rsquare
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE / nrow(df))
  data.frame(RMSE = RMSE, Rsquare = R_square)
}

# Step 1: Data Preprocessing
response_var <- colnames(data)[1]

# Plot histogram of response variable
ggplot(data, aes_string(x = response_var)) + 
  geom_histogram(fill = "gray", color = "black", bins = 30, alpha = 0.6) + 
  theme_minimal() + 
  ggtitle("Distribution of Response Variable") +
  xlab(response_var) + ylab("Count")

# Compute Pearson correlation matrix
cor_matrix <- cor(data, use = "complete.obs", method = "pearson")

# Extract correlations with response variable
cor_target <- cor_matrix[, response_var]

# Select variables highly correlated with response (|r| > 0.3 threshold)
selected_vars <- names(cor_target[abs(cor_target) > 0.3 & names(cor_target) != response_var])

# Ensure at least 2 predictors remain
if (length(selected_vars) < 2) {
  stop("Error: Too few predictors left after correlation filtering!")
}

# Subset dataset with selected variables
data_selected <- data %>% select(all_of(c(response_var, selected_vars)))

# Step 2: Removing Aliased Coefficients (Perfect Collinearity)
alias_info <- alias(lm(as.formula(paste(response_var, "~ .")), data = data_selected))

if (length(alias_info$Complete) > 0) {
  aliased_vars <- rownames(alias_info$Complete)
  cat("Removing aliased variables due to perfect collinearity:", paste(aliased_vars, collapse = ", "), "\n")
  data_selected <- data_selected %>% select(-all_of(aliased_vars))
}

# Step 3: Removing Highly Correlated Predictors (Correlation > 0.85)
cor_matrix_selected <- cor(data_selected[-1], use = "complete.obs", method = "pearson")
highly_correlated <- findCorrelation(cor_matrix_selected, cutoff = 0.85)

if (length(highly_correlated) > 0) {
  cat("Removing highly correlated variables:", colnames(data_selected[-1])[highly_correlated], "\n")
  data_selected <- data_selected %>% select(-highly_correlated)
}

# Ensure at least 2 predictors remain after removing highly correlated variables
if (ncol(data_selected) < 2) {
  stop("Error: Too few predictors left after removing highly correlated variables!")
}

# Step 4: Checking Multicollinearity Using VIF (Variance Inflation Factor)
calculate_vif <- function(df) {
  model <- lm(as.formula(paste(response_var, "~", paste(colnames(df)[-1], collapse = " + "))), data = df)
  vif_values <- vif(model)
  return(vif_values)
}

vif_values <- calculate_vif(data_selected)

while (any(vif_values > 4) & ncol(data_selected) > 2) {
  remove_var <- names(which.max(vif_values))
  cat("Removing variable due to high VIF:", remove_var, "\n")
  data_selected <- data_selected %>% select(-all_of(remove_var))
  vif_values <- calculate_vif(data_selected)
}
#setwd("H:/New folder (2)/B/")
# Export the cleaned and selected data to a CSV file
write.csv(data_selected, "selected_data.csv", row.names = FALSE)

# Step 5: Fit Multiple Linear Regression Model
model <- lm(as.formula(paste(response_var, "~", paste(colnames(data_selected)[-1], collapse = " + "))), data = data_selected)
model_summary <- summary(model)

# Extract Model Coefficients and Generate the Equation
coefficients <- coef(model)
model_equation <- paste0(response_var, " = ", round(coefficients[1], 3), " + ", 
                         paste(paste0(round(coefficients[-1], 3), " * ", names(coefficients[-1])), collapse = " + "))

# Print the Final Model Equation
cat("Final Model Equation:\n", model_equation, "\n")

# Visualize Correlation Matrix
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.8, tl.col = "black")

# Scatterplots of Selected Predictors vs Response Variable
scatter_plots <- list()
for (var in colnames(data_selected)[-1]) {
  p <- ggplot(data_selected, aes_string(x = var, y = response_var)) +
    geom_point(color = "blue", alpha = 0.7) +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    ggtitle(paste("Scatterplot of", var, "vs", response_var))
  scatter_plots <- append(scatter_plots, list(p))
}

# Arrange and display scatterplots
ggarrange(plotlist = scatter_plots, ncol = 2, nrow = ceiling(length(scatter_plots)/2))

# Residuals vs Fitted Values Plot
ggplot(data_selected, aes(x = model$fitted.values, y = model$residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Residuals vs Fitted Values") +
  xlab("Fitted Values") + ylab("Residuals")

# Actual vs Predicted Values Plot
predictions <- predict(model, newdata = data_selected)
ggplot(data_selected, aes(x = data_selected[[response_var]], y = predictions)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  ggtitle("Actual vs Predicted Values") +
  xlab("Actual Values") + ylab("Predicted Values")

# Variable Importance Plot (Absolute t-statistics)
var_importance <- abs(summary(model)$coefficients[-1, "t value"])
importance_df <- data.frame(Variable = names(var_importance), Importance = var_importance)

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  coord_flip() +
  theme_minimal() +
  ggtitle("Variable Importance (based on t-statistics)") +
  xlab("Predictor Variables") + ylab("Importance")



cat("Analysis complete. Graphs generated.\n")
hist_data <- data.frame(Value = data[[response_var]])
write.csv(hist_data, "histogram_data.csv", row.names = FALSE)
write.csv(cor_matrix, "correlation_matrix_data.csv", row.names = TRUE)
scatter_data <- data_selected  # Contains response and selected predictors
write.csv(scatter_data, "scatterplot_data.csv", row.names = FALSE)
residuals_data <- data.frame(Fitted_Values = model$fitted.values, Residuals = model$residuals)
write.csv(residuals_data, "residuals_vs_fitted_data.csv", row.names = FALSE)
predictions <- predict(model, newdata = data_selected)
actual_vs_predicted <- data.frame(Actual = data_selected[[response_var]], Predicted = predictions)
write.csv(actual_vs_predicted, "actual_vs_predicted_data.csv", row.names = FALSE)
var_importance <- abs(summary(model)$coefficients[-1, "t value"])
importance_df <- data.frame(Variable = names(var_importance), Importance = var_importance)
write.csv(importance_df, "variable_importance_data.csv", row.names = FALSE)

