#Steps in a Pairs Trading Strategy

#1.Selection of Pairs

#Choose two stocks that are historically correlated. This can be based on industry, sector, or historical price movements.

#2.Calculating the Spread

#Calculate the spread between the prices of the two stocks. The spread is typically the difference between the prices of the two stocks.

#3.Statistical Analysis

#Compute the mean and standard deviation of the spread.
#Calculate the Z-score of the spread to identify significant deviations from the mean.

#4.Trading Signals

#Buy Signal: When the spread is significantly below the mean (Z-score < -2), buy the underperforming stock and short the outperforming stock.
#Sell Signal: When the spread is significantly above the mean (Z-score > 2), short the underperforming stock and buy the outperforming stock.

#5.Execution

#Execute the trades based on the signals.
#Close the positions when the spread reverts to the mean.

# Load necessary libraries
library(quantmod)
library(TTR)

# Define the stock symbols and the time period
symbol1 <- "AAPL"
symbol2 <- "MSFT"
start_date <- "2020-01-01"
end_date <- "2023-01-01"

# Get stock data
getSymbols(symbol1, from = start_date, to = end_date)
getSymbols(symbol2, from = start_date, to = end_date)

# Extract the closing prices
price1 <- Cl(get(symbol1))
price2 <- Cl(get(symbol2))

# Calculate the spread
spread <- price1 - price2

# Calculate the Z-score of the spread
mean_spread <- rollapply(spread, width = 20, FUN = mean, align = 'right', fill = NA)
sd_spread <- rollapply(spread, width = 20, FUN = sd, align = 'right', fill = NA)
z_score <- (spread - mean_spread) / sd_spread

# Generate trading signals
# Buy the spread when Z-score is below -2 (spread is significantly low)
# Sell the spread when Z-score is above 2 (spread is significantly high)
signal <- ifelse(z_score < -2, 1, ifelse(z_score > 2, -1, 0))

# Remove NA values (due to rolling calculations)
signal <- na.omit(signal)

# Generate trading orders
trade_signal <- diff(signal)

# Buy signal: +1 (spread is significantly below the mean)
# Sell signal: -1 (spread is significantly above the mean)
buy_signals <- which(trade_signal == 1)
sell_signals <- which(trade_signal == -1)

# Create a dataframe to store the signals
trade_df <- data.frame(
  Date = index(spread)[c(buy_signals, sell_signals)],
  Signal = c(rep("Buy", length(buy_signals)), rep("Sell", length(sell_signals)))
)

# Print the trading signals
print(trade_df)

# Plot the spread with trading signals
chartSeries(spread, name = "Spread between AAPL and MSFT")
addTA(z_score, col = "blue", on = NA, type = "l")
addTA(signal == 1, col = "green", on = 1, type = "p", pch = 16, lwd = 2)
addTA(signal == -1, col = "red", on = 1, type = "p", pch = 16, lwd = 2)
abline(h = 0, col = "black")
abline(h = 2, col = "red", lty = 2)
abline(h = -2, col = "green", lty = 2)
