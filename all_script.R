# Install required packages if not installed
#install.packages(c("ggplot2", "dplyr", "ggfortify", "rgl", "ggpubr", "prospectr"))

# Load required libraries
library(ggplot2)
library(dplyr)
library(ggfortify)
library(rgl)
library(ggpubr)
library(prospectr)

# Load dataset (update file path as needed)
data <- read.csv("F:/New folder (2)/A/A.csv")
#data <- read.csv("G:/ABRAR/New Folder9/sugar/input/New folder (2)/A/A.csv")
# Ensure column names are correct
colnames(data) <- make.names(colnames(data))

# Remove first column (assumed ID or non-numeric)
xspace <- data[, -1]

# Apply KenStone algorithm for splitting data (Mahalanobis distance)
ks <- kenStone(as.matrix(xspace), k = 12, metric = "mahal", pc = 0.99, 
               .center = TRUE, .scale = FALSE)

trainid <- ks$test  # Training set IDs

# Assign training and test sets
trainingset <- data[trainid, ]
testset <- data[-trainid, ]

# Extract X (predictors) and Y (response) matrices
x_train <- as.matrix(trainingset[, -1])
y_train <- as.matrix(trainingset[, 1])
x_test <- as.matrix(testset[, -1])
y_test <- as.matrix(testset[, 1])

# Prepare PCA dataset
data_pca <- data
data_pca$Response <- ifelse(1:nrow(data) %in% trainid, "Training", "Test")

# Perform PCA (excluding response column)
pc <- prcomp(data_pca[, -1], scale. = TRUE)

# Summary of PCA
summary(pc)

# Scree plot
plot(pc, type = "lines")

# Prepare tick frame for axis customization
tick_frame <- data.frame(ticks = seq(-20, 20, length.out = 5), zero = 0) %>%
  subset(ticks != 0)

lab_frame <- data.frame(lab = seq(-20, 20), zero = 0) %>%
  subset(lab != 0)

tick_sz <- (tail(lab_frame$lab, 1) - lab_frame$lab[1]) / 128

# Create PCA dataframe
pc_plot <- data.frame(PC1 = pc$x[, 1], PC2 = pc$x[, 2], Response = factor(data_pca$Response))

# PCA Plot
p <- ggplot(pc_plot, aes(x = PC1, y = PC2, color = Response)) +
  labs(x = "PC1", y = "PC2") +
  # Y-axis line
  geom_segment(x = 0, xend = 0, y = min(pc_plot$PC2), yend = max(pc_plot$PC2), size = 1.5, color = "black") +
  # X-axis line
  geom_segment(y = 0, yend = 0, x = min(pc_plot$PC1), xend = max(pc_plot$PC1), size = 1.5, color = "black") +
  # X-ticks
  geom_segment(data = tick_frame, aes(x = ticks, xend = ticks, y = zero, yend = zero + tick_sz), size = 1.5) +
  # Y-ticks
  geom_segment(data = tick_frame, aes(x = zero, xend = zero + tick_sz, y = ticks, yend = ticks), size = 1.5) +
  # Tick labels
  geom_text(data = tick_frame, aes(x = ticks, y = zero, label = ticks), vjust = 1.5, size = 6) +
  geom_text(data = tick_frame, aes(x = zero, y = ticks, label = ticks), hjust = 1.5, size = 6) +
  # Data points
  geom_point(size = 4, alpha = 0.6) +
  scale_color_manual(labels = c("Training set", "Test set"), values = c("dodgerblue", "red2")) +
  # Title
  ggtitle("PCA Analysis") +
  theme_bw() +
  theme(
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 15, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

# Save datasets
write.csv(trainingset, "trainingset.csv", row.names = FALSE)
write.csv(testset, "testset.csv", row.names = FALSE)

# Save PCA plot
ggsave("datasplit.tiff", plot = p, units = "in", width = 6, height = 6, dpi = 600)

# Show plot
print(p)
