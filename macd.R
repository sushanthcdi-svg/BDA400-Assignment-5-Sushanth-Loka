
# BDA400 Assignment 5
# Indicator 3: Moving Average Convergence Divergence (MACD)
# Uses base R only

# Load the EMA function from the existing script
source("ema.R", local = TRUE)

macd <- function(data, short_period, long_period, signal_period) {
  
  # Validate MACD periods
  if (short_period >= long_period) {
    stop("short_period must be less than long_period")
  }
  
  # Calculate the short-term and long-term EMA
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the MACD histogram
  histogram <- macd_line - signal_line
  
  # Return all three indicators as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}

# Test MACD with sample data
data <- c(100, 105, 110, 115, 120, 125, 130)

# Calculate MACD
macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)

# Display results
print(macd_result)
