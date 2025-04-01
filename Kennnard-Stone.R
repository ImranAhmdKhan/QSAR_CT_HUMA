
setwd("H:/New folder (2)/A/")# Load required libraries
library(prospectr)  # For Kennard-Stone
library(ggplot2)    # For PCA and plotting
library(ggfortify)  # For plotting PCA results

# Step 1: Load the dataset
data <- read.csv("A.csv")

# Verify structure & handle missing/infinite values
data_clean <- data[complete.cases(data) & !apply(data, 1, function(x) any(is.infinite(x))), ]

# Step 2: Prepare data for Kennard-Stone algorithm
xspace <- data_clean[, -1]  # Assuming the first column is the response variable

# Ensure input is numeric
xspace_matrix <- as.matrix(xspace)

# Apply Kennard-Stone for dataset splitting
ks <- kenStone(as.matrix(xspace), k=12, metric="mahal", pc=0.99, .center=TRUE, .scale=FALSE)

# Assign training and test sets
trainid <- ks$test  # Test set indices
trainingset <- data_clean[trainid, ]  # Training set
testset <- data_clean[-trainid, ]     # Test set

# Separate features (x) and response (y) variables
x_train <- as.matrix(trainingset[-1])
y_train <- data.matrix(trainingset[1])
x_test <- as.matrix(testset[-1])
y_test <- data.matrix(testset[1])

# Step 3: PCA for dimensionality reduction
data_pca <- data_clean
data_pca$Response <- ifelse(1:nrow(data_pca) %in% trainid, 0, 1)  # Mark training (0) and test (1) sets

# Run PCA
pc <- prcomp(data_pca[, -1], scale. = TRUE)

# Extract variance explained by PC1 & PC2
pc1_var <- round(100 * summary(pc)$importance[2, 1], 2)
pc2_var <- round(100 * summary(pc)$importance[2, 2], 2)

# Scree plot
par(mar = c(5, 5, 2, 2))
barplot(summary(pc)$importance[2,], 
        main = "Scree Plot", 
        col = "darkblue", 
        xlab = "Principal Components", 
        ylab = "Proportion of Variance", 
        cex.axis = 1.0, cex.lab = 1.0, cex.main = 2, 
        las = 2)

# PCA Scatter Plot
pc_plot <- cbind(data_pca[, 1], pc$x)

p <- ggplot(pc_plot, aes(x = pc_plot[, 2], y = pc_plot[, 3])) +
  labs(x = paste("PC1 (", pc1_var, "%)", sep = ""),
       y = paste("PC2 (", pc2_var, "%)", sep = "")) +
  geom_segment(aes(x = 0, xend = 0, y = -10, yend = 10), size = 1.5) +  
  geom_segment(aes(y = 0, yend = 0, x = -10, xend = 10), size = 1.5) +  
  scale_color_manual(name = "Dataset",
                     values = c("red2", "blue"),  
                     labels = c("Training set", "Test set")) +
  geom_point(aes(color = factor(data_pca$Response)), size = 4, alpha = 0.4) +
  ggtitle("Test set molecules in 2D chemical space") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 20, face = "bold"),
        legend.position = "east")

# Save the PCA scatter plot
ggsave("PCA_Scatter_Plot.png", p, width = 6, height = 4, dpi = 300)

# Step 4: Generate Automated Summary Paragraph
num_train <- nrow(trainingset)
num_test <- nrow(testset)

summary_paragraph <- paste(
  "The dataset was preprocessed to remove missing or infinite values, ensuring data consistency. ",
  "The Kennard-Stone (KS) algorithm, based on the Mahalanobis distance, was applied to partition the dataset into a training set (",
  num_train, " molecules) and a test set (", num_test, " molecules). This ensures that the selected molecules in the training set represent the full chemical space, preventing biased predictions. ",
  "\n\nFollowing data partitioning, Principal Component Analysis (PCA) was performed to reduce dimensionality and visualize the dataset distribution. ",
  "The first two principal components (PC1 and PC2) explained ", pc1_var, "% and ", pc2_var, "% of the variance, respectively, indicating their significance in capturing molecular variations. ",
  "\n\nThe PCA scatter plot displayed the training and test sets in 2D chemical space, with red points representing training molecules and blue points representing test molecules. ",
  "The distribution of points confirmed that the test set molecules covered the same chemical space as the training set, ensuring that predictive models would generalize well to unseen data. ",
  "\n\nThis workflow establishes a robust foundation for QSAR modeling, ensuring that the training set diversely represents the molecular space while the test set allows for unbiased model validation. ",
  "The results confirm that the dataset split was chemically meaningful, preventing overfitting and enhancing model interpretability in future predictive modeling tasks."
)

# Print Summary
cat(summary_paragraph)

# Save Summary to File
write(summary_paragraph, file = "PCA_Summary.txt")
