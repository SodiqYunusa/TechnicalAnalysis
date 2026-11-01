# ==============================================================================
# run_tests.R  -  Loads every indicator script and checks it against known values
# Course : BDA400 - Data Science Tools and Techniques (CDI College)
# Author : Sodiq Yunusa
# HOW TO RUN: set the working directory to this folder, then  source("run_tests.R")
#             (or from a terminal:  Rscript run_tests.R)
# Built-in R functions (lm, sd, filter) are used ONLY here, as an independent
# yardstick - never inside the indicator implementations.
# ==============================================================================

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) source(f)

passed <- 0
check <- function(label, condition) {
  if (!isTRUE(condition)) stop("FAILED: ", label)
  passed <<- passed + 1
  cat(sprintf("  [PASS] %s\n", label))
}
close_to <- function(a, b, tol = 1e-8) length(a) == length(b) && all(abs(a - b) < tol, na.rm = TRUE) &&
  identical(is.na(a), is.na(b))

cat("\n=== SMA ===\n")
d <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
r <- sma(d, 3); print(r)
check("SMA length = n - period + 1", length(r) == 7)
check("SMA matches stats::filter", close_to(r, as.numeric(na.omit(stats::filter(d, rep(1/3, 3), sides = 1)))))
check("SMA errors when data shorter than period", inherits(try(sma(1:2, 3), silent = TRUE), "try-error"))
check("SMA period = length(data) gives the mean", close_to(sma(d, 9), mean(d)))

cat("\n=== EMA ===\n")
r <- ema(d, 3); print(r)
check("EMA length = n", length(r) == length(d))
check("EMA first value seeded with data[1]", r[1] == d[1])
check("EMA second value = 10 + (12-10)*0.5 = 11", r[2] == 11)
check("EMA of a constant series is constant", close_to(ema(rep(5, 6), 4), rep(5, 6)))
check("EMA period 1 reproduces the data", close_to(ema(d, 1), d))

cat("\n=== MACD ===\n")
m <- macd(c(100, 105, 110, 115, 120, 125, 130), 3, 5, 2); print(m)
check("MACD returns macd_line, signal_line, histogram",
      identical(names(m), c("macd_line", "signal_line", "histogram")))
check("MACD lengths equal input length", all(sapply(m, length) == 7))
check("MACD histogram = macd_line - signal_line", close_to(m$histogram, m$macd_line - m$signal_line))
check("MACD line = EMA(short) - EMA(long)",
      close_to(m$macd_line, ema(c(100,105,110,115,120,125,130), 3) - ema(c(100,105,110,115,120,125,130), 5)))
check("MACD positive on a steady uptrend", m$macd_line[7] > 0)

cat("\n=== Standard deviation ===\n")
r <- stdev(d); print(r)
check("stdev matches sqrt(mean((x-mean(x))^2))", close_to(r, sqrt(mean((d - mean(d))^2))))
check("stdev matches sd() * sqrt((n-1)/n)", close_to(r, stats::sd(d) * sqrt((length(d) - 1) / length(d))))
check("stdev of a constant vector is 0", stdev(rep(7, 5)) == 0)
check("stdev(c(2,4,4,4,5,5,7,9)) = 2", close_to(stdev(c(2,4,4,4,5,5,7,9)), 2))

cat("\n=== Linear regression ===\n")
lr <- linreg(c(1, 3, 5, 7, 9, 11), 6, 0); print(lr)
check("linreg slope of y = 2x - 1 is 2", close_to(lr$slope, 2))
check("linreg intercept of y = 2x - 1 is -1", close_to(lr$intercept, -1))
check("linreg predicted values reproduce a perfect line", close_to(lr$predicted_values, c(1,3,5,7,9,11)))
set.seed(42); y <- cumsum(rnorm(30, 0.2, 1))
lr <- linreg(y, 10, 0); ref <- coef(lm(y[21:30] ~ seq_len(10)))
check("linreg (L=10, offset 0) matches lm() on the last 10 points", close_to(c(lr$intercept, lr$slope), unname(ref)))
lr <- linreg(y, 10, 3); ref <- coef(lm(y[18:27] ~ seq_len(10)))
check("linreg (L=10, offset 3) matches lm() on points 18-27", close_to(c(lr$intercept, lr$slope), unname(ref)))
check("linreg errors when length > n", inherits(try(linreg(1:5, 10, 0), silent = TRUE), "try-error"))
check("linreg errors when offset >= length", inherits(try(linreg(1:10, 5, 5), silent = TRUE), "try-error"))

cat("\n=== RSI ===\n")
p <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
r <- rsi(p, 5); print(round(r, 4))
check("RSI length = n", length(r) == length(p))
check("RSI first 'period' values are NA", all(is.na(r[1:5])) && !any(is.na(r[6:10])))
check("RSI values lie within 0-100", all(r[6:10] >= 0 & r[6:10] <= 100))
check("RSI of a strictly rising series is 100", all(rsi(1:20, 5)[6:20] == 100))
check("RSI of a strictly falling series is 0", all(rsi(20:1, 5)[6:20] == 0))
# independent hand calculation of the first RSI value (observation 6)
g <- pmax(diff(p), 0); l <- pmax(-diff(p), 0)
check("RSI[6] matches hand calculation", close_to(r[6], 100 - 100 / (1 + mean(g[1:5]) / mean(l[1:5]))))
w <- suppressWarnings(rsi(1:4, 14))
check("RSI with too little data returns all NA", all(is.na(w)))

cat("\n=== Stochastic RSI ===\n")
set.seed(7); px <- 100 + cumsum(rnorm(60))
s <- stoch_rsi(px, 14, 3, 3); print(round(tail(s$k_line, 8), 4)); print(round(tail(s$d_line, 8), 4))
check("StochRSI returns k_line and d_line", identical(names(s), c("k_line", "d_line")))
check("StochRSI vectors are the same length as the data", length(s$k_line) == 60 && length(s$d_line) == 60)
check("%K stays between 0 and 1", all(s$k_line[!is.na(s$k_line)] >= 0 & s$k_line[!is.na(s$k_line)] <= 1))
check("%K first available at observation 14 + 1 + 3 - 1 = 17", all(is.na(s$k_line[1:16])) && !is.na(s$k_line[17]))
check("%D first available two points later (obs 19)", all(is.na(s$d_line[1:18])) && !is.na(s$d_line[19]))
check("%D is the 3-point SMA of %K", close_to(s$d_line[19:60], sma(s$k_line[17:60], 3)))
sw <- suppressWarnings(stoch_rsi(p, 14, 3, 3))
check("StochRSI on the short sample (period 14, 10 points) returns NA, no crash", all(is.na(sw$k_line)))

cat("\n=== Crossover / Crossunder ===\n")
a1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21); a2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
co <- crossover(a1, a2); cu <- crossunder(a1, a2); print(co); print(cu)
check("crossover is TRUE only at index 4 (20 > 18, previously 15 <= 22)", identical(which(co), 4L))
check("crossunder is FALSE everywhere for the sample data", !any(cu))
cu2 <- crossunder(a2, a1)
check("swapping the arrays turns the crossover into a crossunder", identical(which(cu2), 4L))
check("signals are logical, same length as input", is.logical(co) && length(co) == 9 && !co[1])
check("equal-then-above counts as crossover (<= on previous bar)", identical(which(crossover(c(1, 2, 3), c(2, 2, 2))), 3L))
check("crossover/crossunder error on unequal lengths",
      inherits(try(crossover(1:3, 1:4), silent = TRUE), "try-error") &&
      inherits(try(crossunder(1:3, 1:4), silent = TRUE), "try-error"))
check("NA inputs give FALSE rather than crashing", !any(crossover(c(NA, 1, 3), c(2, 2, NA))))

cat(sprintf("\nALL %d CHECKS PASSED\n", passed))
