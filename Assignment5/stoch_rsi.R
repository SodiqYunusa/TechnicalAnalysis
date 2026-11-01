# ==============================================================================
# stoch_rsi.R  -  Stochastic RSI (StochRSI)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries.
# Depends on rsi() (rsi.R) and sma() (sma.R); sourced automatically if missing.
# ==============================================================================

.load_dependency <- function(fn_name, file_name) {
  if (!exists(fn_name, mode = "function")) {
    this_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
    if (is.null(this_dir) || !nzchar(this_dir)) this_dir <- "."
    path <- file.path(this_dir, file_name)
    if (!file.exists(path)) path <- file_name
    source(path)
  }
}
.load_dependency("sma", "sma.R")
.load_dependency("rsi", "rsi.R")

# stoch_rsi(data, period, k_period, d_period)
# Returns list(k_line, d_line). Both vectors have the same length as 'data' and
# are NA wherever there is not yet enough history (RSI warm-up + smoothing).
stoch_rsi <- function(data, period, k_period, d_period) {
  n <- length(data)
  k_line <- rep(NA_real_, n)
  d_line <- rep(NA_real_, n)

  # Calculate the RSI
  rsi_values <- rsi(data, period)
  valid <- which(!is.na(rsi_values))
  if (length(valid) == 0) {
    return(list(k_line = k_line, d_line = d_line))   # rsi() already warned
  }

  # Calculate the StochRSI: (RSI - min RSI) / (max RSI - min RSI)
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  if (max_rsi == min_rsi) {
    k_values <- ifelse(is.na(rsi_values), NA_real_, 0)   # flat RSI: avoid 0/0
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Smooth only the valid (non-NA) stretch, then put it back in position
  first <- valid[1]
  k_valid <- k_values[first:n]

  # Calculate the %K line (StochRSI smoothed with a k_period SMA)
  if (length(k_valid) >= k_period) {
    k_smooth <- sma(k_valid, k_period)
    k_line[(first + k_period - 1):n] <- k_smooth

    # Calculate the %D line (SMA of %K over d_period)
    if (length(k_smooth) >= d_period) {
      d_smooth <- sma(k_smooth, d_period)
      d_start  <- first + k_period - 1 + d_period - 1
      d_line[d_start:n] <- d_smooth
    }
  }

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
