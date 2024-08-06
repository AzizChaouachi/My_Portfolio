#A mean reversion trading algorithm is based on the principle that asset prices tend 
#to revert to their historical mean over time. This strategy identifies when a stock 
#is overbought or oversold, assuming it will revert to its mean. 
#The algorithm typically involves calculating indicators like the Z-score or Bollinger 
#Bands to determine entry and exit points

#This script uses the quantmod package to retrieve financial data and 
#the TTR package to calculate Bollinger Bands.

# Load necessary libraries
library(quantmod)
library(TTR)

# Define the stock symbol and the time period
symbol <- "AAPL"
start_date <- "2020-01-01"
end_date <- "2024-01-01"

# Get stock data
getSymbols(symbol, from = start_date, to = end_date)

# Extract the stock price
stock_data <- get(symbol)

# Define the parameters for Bollinger Bands
n <- 20   # Period for moving average
k <- 2    # Number of standard deviations for the bands

# Calculate Bollinger Bands
bbands <- BBands(Cl(stock_data), n = n, sd = k)
print(bbands)

# Create a trading signal
# Buy signal: Close price is below the lower Bollinger Band
# Sell signal: Close price is above the upper Bollinger Band
signal <- ifelse(Cl(stock_data) < bbands$dn, 1, ifelse(Cl(stock_data) > bbands$up, -1, 0))

# Remove NA values (due to the Bollinger Bands calculation)
signal <- na.omit(signal)

# Generate trading orders
trade_signal <- diff(signal)

# Buy signal: +1 (price crosses below lower Bollinger Band)
# Sell signal: -1 (price crosses above upper Bollinger Band)
buy_signals <- which(trade_signal == 1)
sell_signals <- which(trade_signal == -1)

# Create a dataframe to store the signals
trade_df <- data.frame(
  Date = index(stock_data)[c(buy_signals, sell_signals)],
  Signal = c(rep("Buy", length(buy_signals)), rep("Sell", length(sell_signals)))
)

# Print the trading signals
print(trade_df)

# Plot the stock price with Bollinger Bands and trading signals
chartSeries(stock_data, TA = NULL)
addBBands(n = n, sd = k, on = 1)

# Add buy and sell signals to the plot
addTA(signal == 1, col="red", on = 1, type = "p", pch = 20, lwd = 5)
addTA(signal == -1, col="green", on = 1, type = "p", pch = 20, lwd = 5)
