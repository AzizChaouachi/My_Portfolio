# Load necessary library
library(stats)

# Black-Scholes formula function
black_scholes <- function(S, K, T, r, sigma, type = "call") {
  # S = Current stock price
  # K = Strike price
  # T = Time to expiration (in years)
  # r = Risk-free interest rate
  # sigma = Volatility of the underlying stock
  # type = "call" for call option, "put" for put option
  
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (type == "put") {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Invalid option type. Use 'call' or 'put'.")
  }
  
  return(price)
}

# Function to validate and convert input to numeric
safe_as_numeric <- function(input) {
  num <- as.numeric(input)
  if (is.na(num)) {
    stop("Invalid numeric input. Please enter a valid number.")
  }
  return(num)
}

# Prompt user for input with validation
S <- safe_as_numeric(readline(prompt = "Enter current stock price (S): "))
K <- safe_as_numeric(readline(prompt = "Enter strike price (K): "))
T <- safe_as_numeric(readline(prompt = "Enter time to expiration in years (T): "))
r <- safe_as_numeric(readline(prompt = "Enter risk-free interest rate (r) as a decimal: "))
sigma <- safe_as_numeric(readline(prompt = "Enter volatility (sigma) as a decimal: "))

# Prompt for option type and validate
type <- readline(prompt = "Enter option type ('call' or 'put'): ")
if (!(type %in% c("call", "put"))) {
  stop("Invalid option type. Use 'call' or 'put'.")
}

# Calculate option price
option_price <- black_scholes(S, K, T, r, sigma, type)

# Print result
cat(sprintf("%s Option Price: %.2f\n", toupper(type), option_price))

