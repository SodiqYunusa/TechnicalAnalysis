# ==============================================================================
# crossover.R  -  Crossover detector (arr1 crosses ABOVE arr2)
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# Base R only - no external libraries.
# ==============================================================================

# crossover(arr1, arr2)
# Returns a logical vector the same length as the inputs. Element i is TRUE when
# arr1[i] > arr2[i] AND arr1[i-1] <= arr2[i-1]; otherwise FALSE. The first
# element is always FALSE (no previous point). NA comparisons give FALSE.
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  n <- length(arr1)
  crossover_signals <- rep(FALSE, n)

  # Check for crossovers at each data point
  if (n >= 2) {
    for (i in 2:n) {
      if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
        crossover_signals[i] <- TRUE
      }
    }
  }

  return(crossover_signals)
}
