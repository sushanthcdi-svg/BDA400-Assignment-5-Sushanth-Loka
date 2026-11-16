
# BDA400 Assignment 5
# Indicator 2: Exponential Moving Average (EMA)
# Uses base R only

ema <- function(data, period) {
  
  # Check that the period is valid
  if (length(period) != 1 || is.na(period) ||
      period < 1 || period != floor(period)) {
    stop("Period must be a positive whole number")
  }
  
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # Initialize a vector to store EMA values
  ema_values <- numeric(length(data))
  
  # Check for empty data
  if (length(data) == 0) {
    return(ema_values)
  }
  
  # Loop through the data array
  for (i in seq_along(data)) {
    
    # Calculate EMA for the first data point
    if (i == 1) {
      ema_values[i] <- data[i]
      
    } else {
      
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) *
        multiplier + ema_values[i - 1]
    }
  }
  
  # Return calculated EMA values
  return(ema_values)
}

# Test with sample data
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate EMA with period = 3
ema_result <- ema(data, period = 3)

# Display the results
print(ema_result)
