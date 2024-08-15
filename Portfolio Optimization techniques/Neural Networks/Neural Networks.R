# Install necessary packages
install.packages("neuralnet")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("quadprog")

# Load packages
library(neuralnet)
library(quantmod)
library(PerformanceAnalytics)
library(quadprog)

# Step 1: Load and Prepare Data
# Load historical data for a set of stocks
symbols <- c("AAPL", "MSFT", "GOOG", "AMZN", "TSLA")
getSymbols(symbols, from = "2015-01-01", to = "2024-08-14")

# Calculate daily returns
returns <- na.omit(ROC(merge(Cl(AAPL), Cl(MSFT), Cl(GOOG), Cl(AMZN), Cl(TSLA)), type = "discrete"))
colnames(returns) <- symbols

# Step 2: Split Data into Training and Testing Sets
set.seed(123)
train_size <- round(nrow(returns) * 0.8)
train_data <- returns[1:train_size, ]
test_data <- returns[(train_size + 1):nrow(returns), ]

# Step 3: Train Neural Network Model
# Formula for neural network - predicting returns of each stock
nn_formula <- as.formula(paste("AAPL + MSFT + GOOG + AMZN + TSLA ~", paste(colnames(train_data), collapse = " + ")))

# Train the neural network
set.seed(123)
nn_model <- neuralnet(nn_formula, data = train_data, hidden = c(5, 3), linear.output = TRUE)

# Step 4: Evaluate the Model
# Predict returns on test data
nn_predictions <- compute(nn_model, test_data)$net.result

# Convert predictions to a data frame
nn_predictions <- as.data.frame(nn_predictions)
colnames(nn_predictions) <- symbols
print(nn_predictions)

# Compare predicted vs actual returns
comparison <- cbind(test_data[, 1:5], nn_predictions)
print(head(comparison))

# Step 5: Portfolio Optimization Based on Predictions
# Calculate expected returns and covariance matrix from predictions
expected_returns <- colMeans(nn_predictions)
cov_matrix <- cov(nn_predictions)

# Optimize portfolio using mean-variance optimization
Dmat <- 2 * cov_matrix
dvec <- rep(0, length(symbols))
Amat <- cbind(rep(1, length(symbols)), diag(length(symbols)))
bvec <- c(1, rep(0, length(symbols)))

# Check if the covariance matrix is positive definite
is_positive_definite <- function(matrix) {
  eigen_values <- eigen(matrix)$values
  all(eigen_values > 0)
}

is_positive_definite(Dmat)

# Regularize the covariance matrix by adding a small value to the diagonal
lambda <- 1e-4  # Small positive value
Dmat <- Dmat + lambda * diag(ncol(Dmat))

# Solve quadratic programming problem
portfolio <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)

# Portfolio weights
portfolio_weights <- portfolio$solution
names(portfolio_weights) <- symbols
print("Optimized Portfolio Weights:")
print(portfolio_weights)

# Step 6: Analyze the Optimized Portfolio
# Expected return and risk of the optimized portfolio
portfolio_return <- sum(portfolio_weights * expected_returns)
portfolio_risk <- sqrt(t(portfolio_weights) %*% cov_matrix %*% portfolio_weights)

cat("Expected Portfolio Return:", portfolio_return, "\n")
cat("Portfolio Risk (Standard Deviation):", portfolio_risk, "\n")

# Performance metrics
portfolio_returns <- test_data %*% portfolio_weights
str(portfolio_returns)
# Assuming test_data has rownames as dates or is an xts object
if (!is.xts(test_data)) {
  test_data <- xts(test_data, order.by = as.Date(rownames(test_data)))
}

# Calculate portfolio returns and convert to xts
portfolio_returns <- xts(test_data %*% portfolio_weights, order.by = index(test_data))

charts.PerformanceSummary(portfolio_returns)
