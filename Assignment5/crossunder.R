# ==============================================================================
# crossunder.R  -  Crossunder detector (arr1 crosses BELOW arr2)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries.
# ==============================================================================

# crossunder(arr1, arr2)
# Returns a logical vector the same length as the inputs. Element i is TRUE when
# arr1[i] < arr2[i] AND arr1[i-1] >= arr2[i-1]; otherwise FALSE. The first
# element is always FALSE (no previous point). NA comparisons give FALSE.
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  n <- length(arr1)
  crossunder_signals <- rep(FALSE, n)

  # Check for crossunder signals at each data point
  if (n >= 2) {
    for (i in 2:n) {
      if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossunder_signals[i] <- TRUE
      }
    }
  }

  return(crossunder_signals)
}
