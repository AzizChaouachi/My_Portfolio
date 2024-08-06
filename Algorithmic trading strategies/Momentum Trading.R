#Momentum trading is a strategy that seeks to capitalize on the continuation of 
#existing trends in the market. Traders buy assets that are trending upward and 
#sell them when they show signs of losing momentum. Conversely, they may short assets 
#that are trending downward and cover their positions when momentum wanes.

#ROC= (current price - price N periods ago)/price n periods ago

#Interpretation of ROC

#Positive ROC: When the ROC is positive, it indicates that prices are higher than 
#they were N periods ago, suggesting an uptrend. The higher the ROC, 
#the stronger the upward momentum.

#Negative ROC: When the ROC is negative, it indicates that prices are lower than 
#they were N periods ago, suggesting a downtrend. The lower the ROC, 
#the stronger the downward momentum.

#Zero Line Crossings: When the ROC crosses above the zero line, it generates a buy signal, 
#indicating that momentum has shifted to the upside. Conversely, when the ROC crosses 
#below the zero line, it generates a sell signal, indicating that momentum has shifted 
#to the downside.

"Divergence: Divergence between the price and the ROC can also provide signals. For example,
#if prices are making new highs but the ROC is not, it may indicate a weakening trend and 
potential reversal.

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

# Define the period for the Rate of Change (ROC) indicator
roc_period <- 20

# Calculate the ROC
roc <- ROC(Cl(stock_data), n = roc_period)

# Create a trading signal
# Buy signal: ROC crosses above zero
# Sell signal: ROC crosses below zero
signal <- ifelse(roc > 0, 1, 0)

# Remove NA values (due to the ROC calculation)
signal <- na.omit(signal)

# Generate trading orders
trade_signal <- diff(signal)

# Buy signal: +1 (ROC crosses above zero)
# Sell signal: -1 (ROC crosses below zero)
buy_signals <- which(trade_signal == 1)
sell_signals <- which(trade_signal == -1)

# Create a dataframe to store the signals
trade_df <- data.frame(
  Date = index(stock_data)[c(buy_signals, sell_signals)],
  Signal = c(rep("Buy", length(buy_signals)), rep("Sell", length(sell_signals)))
)

# Print the trading signals
print(trade_df)

# Plot the stock price with ROC and trading signals
chartSeries(stock_data, TA = NULL)
addROC(n = roc_period, col = "blue")

# Add buy and sell signals to the plot
addTA(signal == 1, col = "green", on = 1, type = "p", pch = 16, lwd = 2)
addTA(signal == 0, col = "red", on = 1, type = "p", pch = 16, lwd = 2)

