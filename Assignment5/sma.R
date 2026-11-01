# ==============================================================================
# sma.R  -  Simple Moving Average (SMA)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Project: Technical Analysis using R (Stage 2 - Development Phase)
# Base R only - no external libraries.
# ==============================================================================

# sma(data, period)
#   data   : numeric vector (e.g. closing prices)
#   period : integer, number of points in each window
# Returns a numeric vector of length (length(data) - period + 1).
# Element k is the mean of data[k : (k + period - 1)], i.e. the first value
# belongs to observation number 'period' of the original series.
sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (!is.numeric(period) || length(period) != 1 || period < 1 || period != round(period)) {
    stop("period must be a single positive integer")
  }
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  n_windows  <- length(data) - period + 1
  sma_values <- numeric(n_windows)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_windows) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i]  <- sum(current_window) / period
  }

  return(sma_values)
}
