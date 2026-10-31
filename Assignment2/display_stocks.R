# =====================================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# File    : display_stocks.R
# Author  : Sodiq Yunusa
# Purpose : Display the imported stock data frames, the calculated
#           statistics, and charts of both.
# How to use:
#   1. Set the working directory to the Assignment2 folder
#      (RStudio: Session > Set Working Directory > To Source File Location).
#   2. source("display_stocks.R")   # runs everything from the section
#                                   # "RUN THE ANALYSIS" at the bottom
#   Needs internet access (data is downloaded from Yahoo Finance).
# =====================================================================

# Load the packages and the import / statistics functions.
# (Always sourced, so the script also works in a fresh R session.)
source("stock_functions.R")

# Wider console output so the tables do not wrap
options(width = 120)


# ---------------------------------------------------------------------
# A. Text displays
# ---------------------------------------------------------------------

# Display 1: one stock's data frame - header, first and last rows, structure
display_stock_data <- function(df, symbol, n = 5) {
  line <- strrep("=", 72)
  cat("\n", line, "\n", sep = "")
  cat(" ", symbol, " | ", nrow(df), " trading days | ",
      format(min(df$Date)), " to ", format(max(df$Date)), "\n", sep = "")
  cat(line, "\n", sep = "")
  cat("\nFirst ", n, " rows:\n", sep = "")
  print(head(df, n), row.names = FALSE)
  cat("\nLast ", n, " rows:\n", sep = "")
  print(tail(df, n), row.names = FALSE)
  cat("\nStructure of the data frame:\n")
  str(df)
  invisible(df)
}

# Display 2: Yahoo-Finance-style quote summary, one row per stock
display_quote_summary <- function(stocks) {
  rows <- lapply(names(stocks), function(sym) {
    df  <- stocks[[sym]]
    n   <- nrow(df)
    last_close <- df$Close[n]
    prev_close <- if (n > 1) df$Close[n - 1] else NA_real_
    year <- tail(df, 252)                       # about one trading year
    data.frame(
      Symbol    = sym,
      Date      = format(df$Date[n]),
      Close     = round(last_close, 2),
      Change    = round(last_close - prev_close, 2),
      Change_pct = round(100 * (last_close - prev_close) / prev_close, 2),
      Day_Low   = round(df$Low[n], 2),
      Day_High  = round(df$High[n], 2),
      Wk52_Low  = round(min(year$Low, na.rm = TRUE), 2),
      Wk52_High = round(max(year$High, na.rm = TRUE), 2),
      Volume    = format(df$Volume[n], big.mark = ",", scientific = FALSE),
      stringsAsFactors = FALSE
    )
  })
  quotes <- do.call(rbind, rows)
  cat("\nQUOTE SUMMARY (latest trading day)\n")
  print(quotes, row.names = FALSE)
  invisible(quotes)
}

# Display 3: the statistics table
display_statistics <- function(stats_df) {
  shown <- stats_df
  num_cols <- vapply(shown, is.numeric, logical(1))
  shown[num_cols] <- lapply(shown[num_cols], round, 2)
  cat("\nBASIC STATISTICS OF THE CLOSING PRICE\n")
  print(shown, row.names = FALSE)
  invisible(stats_df)
}


# ---------------------------------------------------------------------
# B. Charts
# ---------------------------------------------------------------------

# Draws a chart on screen (RStudio Plots pane) and, if 'file' is given,
# saves a PNG copy of the same chart.
show_and_save <- function(plot_fn, file = NULL, width = 1100, height = 750) {
  plot_fn()
  if (!is.null(file)) {
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    png(file, width = width, height = height, res = 120)
    plot_fn()
    dev.off()
  }
  invisible(file)
}

# Chart 1: closing price with 20- and 50-day moving averages, volume below
plot_price_chart <- function(df, symbol) {
  d <- add_moving_averages(df, c(20, 50))
  layout(matrix(1:2, nrow = 2), heights = c(3, 1))
  on.exit(layout(1))

  par(mar = c(0.5, 4.5, 3, 1))
  plot(d$Date, d$Close, type = "l", lwd = 2, col = "steelblue",
       xaxt = "n", xlab = "", ylab = "Price (USD)",
       main = paste(symbol, "- Closing Price with Moving Averages"))
  lines(d$Date, d$SMA20, col = "darkorange", lwd = 2)
  lines(d$Date, d$SMA50, col = "firebrick", lwd = 2)
  grid()
  legend("topleft", bty = "n", lwd = 2,
         col = c("steelblue", "darkorange", "firebrick"),
         legend = c("Close", "20-day SMA", "50-day SMA"))

  par(mar = c(4, 4.5, 0.5, 1))
  plot(d$Date, d$Volume / 1e6, type = "h", col = "grey60",
       ylim = c(0, max(d$Volume / 1e6, na.rm = TRUE)),
       xlab = "Date", ylab = "Volume (M)")
}

# Chart 2: candlestick chart (quantmod) of the most recent 'last_n' days
plot_candlestick <- function(df, symbol, last_n = 90) {
  x <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], order.by = df$Date)
  x <- tail(x, last_n)
  chartSeries(x, name = paste(symbol, "- last", nrow(x), "trading days"),
              theme = chartTheme("white"), TA = "addVo();addSMA(n = 20)")
}

# Chart 3: distribution of the closing price with mean, median and mode marked
plot_distribution <- function(df, symbol, stats_row) {
  hist(df$Close, breaks = 25, col = "lightsteelblue", border = "white",
       main = paste(symbol, "- Distribution of Closing Prices"),
       xlab = "Closing price (USD)")
  abline(v = stats_row$Mean,   col = "firebrick",  lwd = 3)
  abline(v = stats_row$Median, col = "navy",       lwd = 3, lty = 2)
  abline(v = stats_row$Mode,   col = "darkgreen",  lwd = 3, lty = 3)
  legend("topright", bty = "n", lwd = 3, lty = c(1, 2, 3),
         col = c("firebrick", "navy", "darkgreen"),
         legend = c(paste("Mean", round(stats_row$Mean, 2)),
                    paste("Median", round(stats_row$Median, 2)),
                    paste("Mode", round(stats_row$Mode, 2))))
}

# Chart 4: statistics compared across the whole portfolio
plot_statistics_comparison <- function(stats_df) {
  par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1))
  on.exit(par(mfrow = c(1, 1)))

  m <- rbind(Mean = stats_df$Mean, Median = stats_df$Median)
  colnames(m) <- stats_df$Symbol
  barplot(m, beside = TRUE, col = c("steelblue", "darkorange"),
          main = "Mean vs Median Closing Price", ylab = "Price (USD)",
          cex.names = 0.8, ylim = c(0, max(m) * 1.2),
          legend.text = TRUE,
          args.legend = list(x = "top", horiz = TRUE, bty = "n"))

  bp <- barplot(stats_df$StdDev, names.arg = stats_df$Symbol, cex.names = 0.8,
                col = "firebrick", main = "Standard Deviation of Closing Price",
                ylab = "Std. deviation (USD)",
                ylim = c(0, max(stats_df$StdDev) * 1.15))
  text(bp, stats_df$StdDev, labels = round(stats_df$StdDev, 1),
       pos = 3, cex = 0.9)
}


# =====================================================================
# RUN THE ANALYSIS  (everything below runs when this file is sourced)
# =====================================================================

# 1. Import the data (one data frame per symbol in the portfolio)
stocks <- load_stock_data("portfolio.txt")

# 2. Calculate the statistics (one summary row per stock)
stats_df <- calculate_all_statistics(stocks)

# 3. Display the data frames, quote summary and statistics
for (sym in names(stocks)) {
  display_stock_data(stocks[[sym]], sym)
}
display_quote_summary(stocks)
display_statistics(stats_df)

# 4. Draw the charts (shown in the Plots pane and saved as PNG files
#    in a 'plots' folder next to this script)
for (sym in names(stocks)) {
  show_and_save(function() plot_price_chart(stocks[[sym]], sym),
                file = file.path("plots", paste0(sym, "_price_chart.png")))
}
show_and_save(function() plot_candlestick(stocks[[1]], names(stocks)[1]),
              file = file.path("plots", paste0(names(stocks)[1], "_candlestick.png")))
show_and_save(function() plot_distribution(stocks[[1]], names(stocks)[1],
                                           stats_df[1, ]),
              file = file.path("plots", paste0(names(stocks)[1], "_distribution.png")))
show_and_save(function() plot_statistics_comparison(stats_df),
              file = file.path("plots", "statistics_comparison.png"))

# 5. Save the statistics table next to the script as well
write.csv(stats_df, "portfolio_statistics.csv", row.names = FALSE)
cat("\nDone. Charts saved in the 'plots' folder; statistics saved to portfolio_statistics.csv\n")
