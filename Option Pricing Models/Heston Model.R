# Load necessary libraries
library(ggplot2)

# Heston model simulation function
heston_simulation <- function(S0, V0, T, mu, kappa, theta, sigma_V, rho, n_sim = 10, n_steps = 100) {
  dt <- T / n_steps  # Time step
  times <- seq(0, T, length.out = n_steps + 1)  # Time points
  all_paths <- matrix(0, nrow = n_steps + 1, ncol = n_sim)  # Matrix to store stock price paths
  vol_paths <- matrix(0, nrow = n_steps + 1, ncol = n_sim)  # Matrix to store volatility paths
  
  all_paths[1, ] <- S0
  vol_paths[1, ] <- V0
  
  for (i in 2:(n_steps + 1)) {
    Z1 <- rnorm(n_sim)  # Standard normal random variables
    Z2 <- rnorm(n_sim)  # Standard normal random variables
    
    # Correlated random variables
    Z2 <- rho * Z1 + sqrt(1 - rho^2) * Z2
    
    # Update stock price and volatility
    vol_paths[i, ] <- pmax(vol_paths[i - 1, ] + kappa * (theta - vol_paths[i - 1, ]) * dt + sigma_V * sqrt(vol_paths[i - 1, ]) * sqrt(dt) * Z2, 0)
    all_paths[i, ] <- all_paths[i - 1, ] * exp((mu - 0.5 * vol_paths[i - 1, ]) * dt + sqrt(vol_paths[i - 1, ]) * sqrt(dt) * Z1)
  }
  
  # Create data frames for plotting
  stock_results <- data.frame(
    Time = rep(times, each = n_sim),
    StockPrice = c(all_paths),
    Path = rep(1:n_sim, times = n_steps + 1)
  )
  
  vol_results <- data.frame(
    Time = rep(times, each = n_sim),
    Volatility = c(vol_paths),
    Path = rep(1:n_sim, times = n_steps + 1)
  )
  
  return(list(stock_results = stock_results, vol_results = vol_results))
}

# Example parameters
S0 <- 100    # Initial stock price
V0 <- 0.04   # Initial volatility
T <- 1       # Time to expiration (1 year)
mu <- 0.05   # Drift (5%)
kappa <- 2.0 # Rate of mean reversion
theta <- 0.04 # Long-term mean volatility
sigma_V <- 0.5 # Volatility of volatility
rho <- -0.7  # Correlation between stock price and volatility
n_sim <- 10  # Number of paths to simulate
n_steps <- 100 # Number of time steps

# Run Heston model simulation
simulation_result <- heston_simulation(S0, V0, T, mu, kappa, theta, sigma_V, rho, n_sim, n_steps)

# Plot simulated stock prices
ggplot(simulation_result$stock_results, aes(x = Time, y = StockPrice, group = Path)) +
  geom_line(alpha = 0.5, color = "blue") +
  labs(title = "Simulated Paths of Stock Prices",
       x = "Time",
       y = "Stock Price") +
  theme_minimal()

# Plot simulated volatilities
ggplot(simulation_result$vol_results, aes(x = Time, y = Volatility, group = Path)) +
  geom_line(alpha = 0.5, color = "red") +
  labs(title = "Simulated Paths of Volatility",
       x = "Time",
       y = "Volatility") +
  theme_minimal()
