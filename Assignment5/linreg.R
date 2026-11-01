# ==============================================================================
# linreg.R  -  Linear Regression over a rolling window of a series
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries (lm() is deliberately not used).
# ==============================================================================

# linreg(regressionSource, regressionLength, regressionOffset)
#   regressionSource : numeric vector (e.g. closing prices)
#   regressionLength : number of points in the regression window
#   regressionOffset : how many of the most recent points to skip (0 = the
#                      window ends at the last observation)
# Returns list(slope, intercept, predicted_values). The x-values are 1..L, so
# 'predicted_values' has regressionLength elements (one per point in the window).
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  if (regressionLength < 2) {
    stop("regressionLength must be at least 2")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  if (regressionOffset < 0) {
    stop("regressionOffset cannot be negative")
  }

  # Calculate the ending / starting index for the regressionSource
  # (window = 'regressionLength' points that finish 'regressionOffset' points
  #  before the end of the series)
  end_index   <- min(n, n - regressionOffset)
  start_index <- max(1, end_index - regressionLength + 1)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]
  if (length(source_subset) < 2) {
    stop("Not enough data points in the selected window to fit a line")
  }

  # Calculate the index values for the regression points
  index_values <- seq_along(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index  <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(
    slope            = slope,
    intercept        = intercept,
    predicted_values = predicted_values
  )
  return(result)
}
