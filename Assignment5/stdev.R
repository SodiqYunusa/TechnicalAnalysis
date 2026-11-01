# ==============================================================================
# stdev.R  -  Standard Deviation (population form, divides by n)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries (sd() and var() are deliberately not used).
# ==============================================================================

# stdev(data)
#   data : numeric vector
# Returns a single number: sqrt( sum((x - mean)^2) / n ).
# NOTE: this is the POPULATION standard deviation required by the assignment
# formula, so it differs slightly from R's built-in sd(), which divides by n - 1.
stdev <- function(data) {
  n <- length(data)
  if (n == 0) stop("data must contain at least one value")

  # Calculate the mean of the data
  mean_value <- sum(data) / n

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values * diff_values

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
