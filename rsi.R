
# BDA400 Assignment 5
# Indicator 6: Relative Strength Index (RSI)
# Uses base R only

rsi <- function(data, period) {
  
  # Check that the period is valid
  if (length(period) != 1 || is.na(period) ||
      !is.numeric(period) || period < 1 ||
      period != floor(period)) {
    stop("Period must be a positive whole number")
  }
  
  # Check that enough price data is available
  if (length(data) <= period) {
    stop("Data must contain more values than the period")
  }
  
  # Calculate differences between consecutive prices
  diff_values <- diff(data)
  
  # Initialize vectors for gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else if (diff_values[i] < 0) {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # Calculate initial average gains and losses
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))
  
  # Calculate initial RSI using the first period
  if (avg_loss == 0) {
    if (avg_gain == 0) {
      rsi_values[period + 1] <- 50
    } else {
      rsi_values[period + 1] <- 100
    }
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - (100 / (1 + rs))
  }
  
  # Calculate subsequent RSI values using Wilder smoothing
  if (length(data) > period + 1) {
    for (i in (period + 2):length(data)) {
      
      avg_gain <- (avg_gain * (period - 1) +
                     gains[i - 1]) / period
      
      avg_loss <- (avg_loss * (period - 1) +
                     losses[i - 1]) / period
      
      if (avg_loss == 0) {
        if (avg_gain == 0) {
          rsi_values[i] <- 50
        } else {
          rsi_values[i] <- 100
        }
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - (100 / (1 + rs))
      }
    }
  }
  
  # Return RSI values
  return(rsi_values)
}

# Test with sample data from assignment
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

# Calculate RSI with period = 5
rsi_result <- rsi(data, period = 5)

# Display results
print(rsi_result)
