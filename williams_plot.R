# Set working directory
setwd("H:/New folder (2)/A/")

# Load necessary libraries
library(prospectr)

# Load data
data <- read.csv("selected_data.csv")

# Create training and test set IDs using Kennard-Stone on Mahalanobis distance
xspace <- data[, -1]
ks <- kenStone(as.matrix(xspace), k = 10, metric = "mahal", pc = 0.99, .center = TRUE, .scale = FALSE)

trainid <- ks$test
id <- seq(1, nrow(data), by = 1)
testid <- setdiff(id, trainid)

# Assign training and test sets
trainingset <- data[trainid, ]
testset <- data[testid, ]

x_train <- as.matrix(trainingset[, -1])
y_train <- trainingset[, 1]

x_test <- as.matrix(testset[, -1])
y_test <- testset[, 1]

# ---- Linear Model ----
#mdl <- lm(Response ~ ndonr + Shet + GATSe1 + MATSm4 + MATSm5, data = trainingset)
#summary(mdl)
mdl <- lm(Response~1 + Lowest.Frequency..cm.1. + Balaban.type.index.from.van.der.waals.weighted.distance.matrix + Wiener.type.index.from.van.der.waals.weighted.distance.matrix + AlogP + HOMO.1, data = trainingset)
summary(mdl)

# ---- Williams Plot Calculation ----
#wp_x <- as.matrix(data[, c("ndonr", "Shet", "GATSe1", "MATSm4", "MATSm5")])
wp_x <- as.matrix(data[, c("Lowest.Frequency..cm.1.", "Balaban.type.index.from.van.der.waals.weighted.distance.matrix", "Wiener.type.index.from.van.der.waals.weighted.distance.matrix", "AlogP", "HOMO.1")])
# Calculate leverage (hat values)
h <- diag(wp_x %*% solve(t(wp_x) %*% wp_x) %*% t(wp_x))

# Standardized residuals
stdres_train <- residuals(mdl) / sd(residuals(mdl))
res_test <- predict(mdl, newdata = testset) - testset$Response
stdres_test <- res_test / sd(residuals(mdl))

# ---- Create Williams Plot Data ----
wp_mt <- data.frame(id = seq(1, nrow(data)), hatvalue = h, stdres = NA, set = "Test")
wp_mt$stdres[trainid] <- stdres_train
wp_mt$stdres[testid] <- stdres_test
wp_mt$set[trainid] <- "Train"

# ---- Plot Williams Plot ----
plot(wp_mt$hatvalue, wp_mt$stdres, pch = 19, cex = 1.5,
     col = ifelse(wp_mt$set == "Train", "blue", "red"), 
     xlim = c(0, 0.3), ylim = c(-4, 4),
     xlab = "Hat Values", ylab = "Standardized Residuals",
     main = "Williams Plot for lnKD Model")

# Add threshold lines
abline(h = c(-3, 3), lty = 2, col = "black")  # Standardized residual threshold
abline(v = 2 * mean(wp_mt$hatvalue), lty = 2, col = "black")  # Leverage threshold

# Add legend
legend("topright", legend = c("Training Set", "Test Set"), col = c("blue", "red"), pch = 19, cex = 1.2)
