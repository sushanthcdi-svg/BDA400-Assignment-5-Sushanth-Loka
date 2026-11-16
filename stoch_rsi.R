
# BDA400 Assignment 5
# Indicator 7: Stochastic RSI (StochRSI)
# Uses base R only

# Load previously implemented functions
source("rsi.R", local = TRUE)
source("sma.R", local = TRUE)

stoch_rsi <- function(data, period, k_period, d_period) {
  
  # Validate the periods
  periods <- c(period, k_period, d_period)
  
  if (!is.numeric(periods) ||
      anyNA(periods) ||
      any(!is.finite(periods)) ||
      any(periods < 1) ||
      any(periods != floor(periods))) {
    stop("All periods must be positive whole numbers")
  }
  
  # Calculate RSI using the existing function
  rsi_values <- rsi(data, period)
  
  # Remove initial NA values
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  # Ensure enough values for both moving averages
  required_length <- k_period + d_period - 1
  
  if (length(valid_rsi) < required_length) {
    stop("Not enough RSI values for K and D periods")
  }
  
  # Find minimum and maximum RSI values
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  
  # Normalize RSI values between 0 and 1
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(valid_rsi))
  } else {
    k_values <- (valid_rsi - min_rsi) /
      (max_rsi - min_rsi)
  }
  
  # Calculate the K line using SMA
  k_line <- sma(k_values, k_period)
  
  # Calculate the D line using SMA of K
  d_line <- sma(k_line, d_period)
  
  # Return the K and D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}

# Test data
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

# Calculate Stochastic RSI
stoch_rsi_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)

# Display the results
print(stoch_rsi_result)
