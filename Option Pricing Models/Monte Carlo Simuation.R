# Load necessary libraries
library(stats)
library(ggplot2)

# Monte Carlo simulation function with visualization
monte_carlo_simulation <- function(S0, K, T, r, sigma, n_sim = 100, n_steps = 10) {
  dt <- T / n_steps  # Time step
  times <- seq(0, T, length.out = n_steps + 1)  # Time points
  all_paths <- matrix(0, nrow = n_steps + 1, ncol = n_sim)  # Matrix to store all paths
  
  # Initial stock price
  all_paths[1, ] <- S0
  
  for (i in 2:(n_steps + 1)) {
    Z <- rnorm(n_sim)  # Standard normal random variables
    # Update stock prices
    all_paths[i, ] <- all_paths[i - 1, ] * exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
  }
  
  # Final stock prices and call option payoffs
  final_prices <- all_paths[n_steps + 1, ]
  payoffs <- pmax(final_prices - K, 0) # Call option payoff
  
  # Discount payoffs back to present value
  option_price <- mean(payoffs) * exp(-r * T)
  
  # Create data frames for plotting
  paths_result <- data.frame(
    Time = rep(times, each = n_sim),
    StockPrice = c(all_paths),
    Path = rep(1:n_sim, times = n_steps + 1)
  )
  
  final_prices_df <- data.frame(
    FinalStockPrice = final_prices
  )
  
  payoffs_df <- data.frame(
    Payoff = payoffs
  )
  
  return(list(
    option_price = option_price,
    paths_result = paths_result,
    final_prices_df = final_prices_df,
    payoffs_df = payoffs_df
  ))
}

# Example parameters
S0 <- 100    # Initial stock price
K <- 100     # Strike price
T <- 1       # Time to expiration (1 year)
r <- 0.05    # Risk-free interest rate (5%)
sigma <- 0.2 # Volatility (20%)
n_sim <- 100 # Number of simulations
n_steps <- 10 # Number of time steps

# Run Monte Carlo simulation
simulation_result <- monte_carlo_simulation(S0, K, T, r, sigma, n_sim, n_steps)

# Print option price
cat(sprintf("Estimated Call Option Price: %.2f\n", simulation_result$option_price))

# Plot histogram of final stock prices
ggplot(simulation_result$final_prices_df, aes(x = FinalStockPrice)) +
  geom_histogram(bins = 50, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Final Stock Prices",
       x = "Final Stock Price",
       y = "Frequency") +
  theme_minimal()

# Plot histogram of call option payoffs
ggplot(simulation_result$payoffs_df, aes(x = Payoff)) +
  geom_histogram(bins = 50, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Call Option Payoffs",
       x = "Payoff",
       y = "Frequency") +
  theme_minimal()

# Plot simulated paths over time
ggplot(simulation_result$paths_result, aes(x = Time, y = StockPrice, group = Path)) +
  geom_line(alpha = 0.3, color = "blue") +
  labs(title = "Simulated Paths of Stock Prices Over Time",
       x = "Time",
       y = "Stock Price") +
  theme_minimal()




