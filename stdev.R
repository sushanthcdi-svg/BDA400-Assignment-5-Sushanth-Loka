
# BDA400 Assignment 5
# Indicator 4: Standard Deviation (stdev)
# Uses base R only

stdev <- function(data) {
  
  # Check if the data is empty
  if (length(data) == 0) {
    stop("Data must contain at least one value")
  }
  
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)
  
  # Calculate differences from the mean
  diff_values <- data - mean_value
  
  # Calculate the squared differences
  squared_diff <- diff_values^2
  
  # Calculate the variance
  variance <- sum(squared_diff) / length(data)
  
  # Calculate standard deviation
  standard_deviation <- sqrt(variance)
  
  # Return the standard deviation
  return(standard_deviation)
}

# Test with sample data
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate standard deviation
stdev_result <- stdev(data)

# Display the result
print(stdev_result)
