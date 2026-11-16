
# BDA400 Assignment 5
# Indicator 8: Crossover Function
# Uses base R only

crossover <- function(arr1, arr2) {
  
  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize crossover signals
  crossover_signals <- rep("None", length(arr1))
  
  # Handle arrays with fewer than two values
  if (length(arr1) < 2) {
    return(crossover_signals)
  }
  
  # Check for crossovers at each data point
  for (i in 2:length(arr1)) {
    
    # Check for upward crossover
    if (arr1[i] > arr2[i] &&
        arr1[i - 1] <= arr2[i - 1]) {
      
      crossover_signals[i] <- "Up"
      
      # Check for downward crossover
    } else if (arr1[i] < arr2[i] &&
               arr1[i - 1] >= arr2[i - 1]) {
      
      crossover_signals[i] <- "Down"
      
    } else {
      
      # No crossover detected
      crossover_signals[i] <- "None"
    }
  }
  
  # Return the crossover signals
  return(crossover_signals)
}

# Test using sample data from assignment
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

# Calculate crossover signals
crossover_result <- crossover(arr1, arr2)

# Display results
print(crossover_result)
