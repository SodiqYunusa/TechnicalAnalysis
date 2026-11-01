# ==============================================================================
# rsi.R  -  Relative Strength Index (Wilder's smoothing)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries.
# ==============================================================================

# rsi(data, period)
#   data   : numeric vector of prices
#   period : integer, look-back (typically 14)
# Returns a numeric vector the same length as 'data'. The first 'period' values
# are NA (not enough history). If length(data) <= period the result is all NA
# and a warning is issued.
rsi <- function(data, period) {
  if (!is.numeric(period) || length(period) != 1 || period < 1 || period != round(period)) {
    stop("period must be a single positive integer")
  }
  n <- length(data)

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, n)

  if (n <= period) {
    warning("Not enough data: need more than 'period' observations to compute RSI")
    return(rsi_values)
  }

  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period' changes
  avg_gain <- sum(gains[1:period])  / period
  avg_loss <- sum(losses[1:period]) / period

  # helper: convert average gain / loss into an RSI value
  to_rsi <- function(g, l) {
    if (l == 0 && g == 0) return(50)   # completely flat: neutral
    rs <- g / l                        # l == 0 gives Inf -> RSI = 100
    100 - (100 / (1 + rs))
  }

  # First RSI value sits at observation period + 1 (seeded with simple averages)
  rsi_values[period + 1] <- to_rsi(avg_gain, avg_loss)

  # Calculate RSI values using Wilder's smoothing method
  if (n > period + 1) {
    for (i in (period + 2):n) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1])  / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      rsi_values[i] <- to_rsi(avg_gain, avg_loss)
    }
  }

  return(rsi_values)
}
