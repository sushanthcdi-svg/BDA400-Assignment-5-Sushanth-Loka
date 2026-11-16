
# BDA400 Assignment 5
# Step 11: Final Testing of All Nine Indicators
# Uses base R only

cat("BDA400 - FINAL FUNCTION TESTING\n")
cat("--------------------------------\n")

# Load all nine functions
source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("\nAll nine R scripts loaded.\n\n")

# Helper function to compare numeric results
check_numeric <- function(actual, expected) {
  isTRUE(all.equal(
    as.numeric(actual),
    as.numeric(expected),
    tolerance = 0.00001
  ))
}

# Test 1: Simple Moving Average
sma_result <- sma(c(10, 12, 15, 20, 18, 22,
                    25, 24, 21), 3)

stopifnot(check_numeric(
  sma_result,
  c(12.333333, 15.666667, 17.666667,
    20, 21.666667, 23.666667, 23.333333)
))
cat("TEST 1 PASSED: SMA\n")

# Test 2: Exponential Moving Average
ema_result <- ema(c(10, 12, 15, 20, 18,
                    22, 25, 24, 21), 3)

stopifnot(check_numeric(
  ema_result,
  c(10, 11, 13, 16.5, 17.25, 19.625,
    22.3125, 23.15625, 22.078125)
))
cat("TEST 2 PASSED: EMA\n")

# Test 3: MACD
macd_result <- macd(
  c(100, 105, 110, 115, 120, 125, 130),
  3, 5, 2
)

stopifnot(
  length(macd_result$macd_line) == 7,
  length(macd_result$signal_line) == 7,
  length(macd_result$histogram) == 7,
  check_numeric(
    tail(macd_result$macd_line, 1),
    4.200210048
  ),
  check_numeric(
    macd_result$histogram,
    macd_result$macd_line -
      macd_result$signal_line
  )
)
cat("TEST 3 PASSED: MACD\n")

# Test 4: Standard Deviation
stdev_result <- stdev(
  c(10, 12, 15, 20, 18, 22, 25, 24, 21)
)

stopifnot(check_numeric(
  stdev_result, 4.946628731
))
cat("TEST 4 PASSED: Standard Deviation\n")

# Test 5: Linear Regression
linreg_result <- linreg(
  c(10, 12, 14, 16, 18), 5, 0
)

stopifnot(
  check_numeric(linreg_result$slope, 2),
  check_numeric(linreg_result$intercept, 8),
  check_numeric(
    linreg_result$predicted_values,
    c(10, 12, 14, 16, 18)
  )
)
cat("TEST 5 PASSED: Linear Regression\n")

# Test 6: Relative Strength Index
rsi_result <- rsi(
  c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62),
  5
)

stopifnot(
  length(rsi_result) == 10,
  all(is.na(rsi_result[1:5])),
  check_numeric(
    rsi_result[6:10],
    c(60, 74.4, 76.7272727,
      81.8761062, 70.2239089)
  )
)
cat("TEST 6 PASSED: RSI\n")

# Test 7: Stochastic RSI
stoch_result <- stoch_rsi(
  c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62),
  5, 3, 3
)

stopifnot(
  check_numeric(
    stoch_result$k_line,
    c(0.47429636, 0.80762970, 0.74399724)
  ),
  check_numeric(
    stoch_result$d_line,
    0.67530777
  )
)
cat("TEST 7 PASSED: Stochastic RSI\n")

# Test 8: Crossover
crossover_result <- crossover(
  c(1, 3, 1, 4),
  c(2, 2, 2, 2)
)

stopifnot(identical(
  crossover_result,
  c("None", "Up", "Down", "Up")
))
cat("TEST 8 PASSED: Crossover\n")

# Test 9: Crossunder
crossunder_result <- crossunder(
  c(5, 7, 9, 6, 4),
  c(6, 6, 6, 6, 6)
)

stopifnot(identical(
  crossunder_result,
  c("None", "False", "False", "False", "True")
))
cat("TEST 9 PASSED: Crossunder\n")

# Final confirmation
cat("\n--------------------------------\n")
cat("ALL 9 TESTS PASSED SUCCESSFULLY!\n")
cat("BDA400 Assignment 5 testing complete.\n")
