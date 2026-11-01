# ==============================================================================
# ema.R  -  Exponential Moving Average (EMA)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries.
# ==============================================================================

# ema(data, period)
#   data   : numeric vector
#   period : integer, EMA period (smoothing factor = 2 / (period + 1))
# Returns a numeric vector the same length as 'data'. The first value is seeded
# with data[1], as specified in the assignment pseudocode.
ema <- function(data, period) {
  if (!is.numeric(period) || length(period) != 1 || period < 1) {
    stop("period must be a single positive number")
  }

  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values
  n          <- length(data)
  ema_values <- numeric(n)

  # Loop through the data array
  for (i in seq_len(n)) {
    if (i == 1) {
      # Calculate EMA for the first data point
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}
