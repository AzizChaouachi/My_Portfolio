# Load necessary libraries
library(quantmod)
library(TTR)

# Define the stock symbol and the time period
symbol <- "AAPL"
start_date <- "2020-01-01"
end_date <- "2023-01-01"

# Get stock data
getSymbols(symbol, from = start_date, to = end_date)

# Extract the stock price
stock_data <- get(symbol)
print(stock_data)

# Define the short-term and long-term moving average periods
short_term <- 50
long_term <- 200

# Calculate the moving averages
short_ma <- SMA(Cl(stock_data), n = short_term)
long_ma <- SMA(Cl(stock_data), n = long_term)
print(short_ma)
print(long_ma)

# Create a trading signal
# Buy signal: short MA crosses above long MA
# Sell signal: short MA crosses below long MA
signal <- ifelse(short_ma > long_ma, 1, 0)

# Remove NA values (due to the moving average calculation)
signal <- na.omit(signal)

# Generate trading orders
trade_signal <- diff(signal)

# Buy signal: +1 (short MA crosses above long MA)
# Sell signal: -1 (short MA crosses below long MA)
buy_signals <- which(trade_signal == 1)
sell_signals <- which(trade_signal == -1)

# Create a dataframe to store the signals
trade_df <- data.frame(
  Date = index(stock_data)[c(buy_signals, sell_signals)],
  Signal = c(rep("Buy", length(buy_signals)), rep("Sell", length(sell_signals)))
)

# Print the trading signals
print(trade_df)

# Plot the stock price with moving averages and trading signals
chartSeries(stock_data, TA = NULL)
addSMA(n = short_term, col = "blue")
addSMA(n = long_term, col = "red")

# Add buy and sell signals to the plot
addTA(signal == 1, col = "green", on = 1, type = "p", pch = 16, lwd = 2)
addTA(signal == 0, col = "red", on = 1, type = "p", pch = 16, lwd = 2)
