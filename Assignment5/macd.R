# ==============================================================================
# macd.R  -  Moving Average Convergence Divergence (MACD)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries.
# Depends on ema() from ema.R (sourced automatically if not yet loaded).
# ==============================================================================

if (!exists("ema", mode = "function")) {
  this_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")
  if (is.null(this_dir) || !nzchar(this_dir)) this_dir <- "."
  ema_path <- file.path(this_dir, "ema.R")
  if (!file.exists(ema_path)) ema_path <- "ema.R"
  source(ema_path)
}

# macd(data, short_period, long_period, signal_period)
# Returns a list with three numeric vectors (same length as 'data'):
#   macd_line, signal_line, histogram
macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line   = macd_line,
    signal_line = signal_line,
    histogram   = histogram
  )
  return(result)
}
