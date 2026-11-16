
# BDA400 Assignment 5
# Indicator 5: Linear Regression (linreg)
# Uses base R only

linreg <- function(regressionSource, regressionLength,
                   regressionOffset) {
  
  # Calculate the total number of data elements
  n <- length(regressionSource)
  
  # Check if regressionLength is valid
  if (length(regressionLength) != 1 ||
      is.na(regressionLength) ||
      regressionLength < 2 ||
      regressionLength != floor(regressionLength)) {
    stop("regressionLength must be a whole number of at least 2")
  }
  
  # Check if regressionLength exceeds the data length
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # Check if regressionOffset is valid
  if (length(regressionOffset) != 1 ||
      is.na(regressionOffset) ||
      regressionOffset < 0 ||
      regressionOffset != floor(regressionOffset) ||
      regressionOffset >= regressionLength) {
    stop("regressionOffset must be a non-negative whole number less than regressionLength")
  }
  
  # Calculate starting index
  start_index <- max(
    1, n - regressionLength + regressionOffset
  )
  
  # Calculate ending index
  end_index <- min(n, n - regressionOffset)
  
  # Extract the relevant portion of the data
  source_subset <- regressionSource[start_index:end_index]
  
  # Calculate index values for regression points
  index_values <- seq_along(source_subset)
  
  # Check if enough observations are available
  if (length(source_subset) < 2) {
    stop("At least two regression points are required")
  }
  
  # Calculate the sum of index values
  sum_index <- sum(index_values)
  
  # Calculate the sum of source values
  sum_source <- sum(source_subset)
  
  # Calculate the mean of index values
  mean_index <- sum_index / length(index_values)
  
  # Calculate the mean of source values
  mean_source <- sum_source / length(source_subset)
  
  # Calculate numerator for the regression formula
  numerator <- sum(
    (index_values - mean_index) *
      (source_subset - mean_source)
  )
  
  # Calculate denominator for the regression formula
  denominator <- sum(
    (index_values - mean_index)^2
  )
  
  # Calculate slope
  slope <- numerator / denominator
  
  # Calculate intercept
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return results as a list
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}

# Test the Linear Regression function
regressionSource <- c(10, 12, 14, 16, 18)

# Calculate Linear Regression
linreg_result <- linreg(
  regressionSource,
  regressionLength = 5,
  regressionOffset = 0
)

# Display results
print(linreg_result)
