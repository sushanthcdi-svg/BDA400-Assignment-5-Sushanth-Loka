
# BDA400 Assignment 5
# Indicator 9: Crossunder Function
# Uses base R only

crossunder <- function(arr1, arr2) {
  
  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize crossunder signals
  crossunder_signals <- rep("False", length(arr1))
  
  # Handle empty arrays
  if (length(arr1) == 0) {
    return(crossunder_signals)
  }
  
  # First position has no previous value
  crossunder_signals[1] <- "None"
  
  # Handle arrays with only one value
  if (length(arr1) == 1) {
    return(crossunder_signals)
  }
  
  # Check for crossunder events
  for (i in 2:length(arr1)) {
    
    # Check if arr1 crosses below arr2
    if (arr1[i] < arr2[i] &&
        arr1[i - 1] >= arr2[i - 1]) {
      
      crossunder_signals[i] <- "True"
      
    } else {
      
      # No crossunder detected
      crossunder_signals[i] <- "False"
    }
  }
  
  # Return the crossunder signals
  return(crossunder_signals)
}

# Test data from the assignment
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

# Calculate crossunder signals
crossunder_result <- crossunder(arr1, arr2)

# Display results
print(crossunder_result)
