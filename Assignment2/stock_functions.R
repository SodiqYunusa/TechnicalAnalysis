# =====================================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# File    : stock_functions.R
# Author  : Sodiq Yunusa
# Purpose : Functions to
#             (1) read the stock symbols listed in portfolio.txt,
#             (2) import the stock data for each symbol with quantmod,
#             (3) calculate basic statistics for each stock.
# How to use:
#   1. Set the working directory to the Assignment2 folder
#      (RStudio: Session > Set Working Directory > To Source File Location).
#   2. source("stock_functions.R")
#   3. stocks   <- load_stock_data("portfolio.txt")
#      stats_df <- calculate_all_statistics(stocks)
# =====================================================================

suppressPackageStartupMessages({
  library(quantmod)   # getSymbols(): downloads the market data
  library(TTR)        # SMA(): simple moving average
})


# ---------------------------------------------------------------------
# 1. Read the portfolio file
# ---------------------------------------------------------------------
# Reads portfolio.txt (one stock symbol per line) and returns a clean
# character vector of upper-case symbols. Blank lines, lines starting
# with '#', and duplicate symbols are ignored.
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) {
    stop("Portfolio file not found: '", file, "'. ",
         "Set the working directory to the Assignment2 folder first.")
  }
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  symbols <- unique(toupper(symbols))
  if (length(symbols) == 0) {
    stop("No stock symbols were found in '", file, "'.")
  }
  symbols
}


# ---------------------------------------------------------------------
# 2. Import the stock data
# ---------------------------------------------------------------------
# Downloads the price history of ONE symbol with quantmod and returns
# it as an xts object (columns: Symbol.Open, .High, .Low, .Close,
# .Volume, .Adjusted). Kept as its own small function so the data
# source can be changed (or swapped for test data) in one place.
fetch_one_symbol <- function(symbol, from, to, src = "yahoo") {
  getSymbols(symbol, src = src, from = from, to = to, auto.assign = FALSE)
}

# Converts the xts object returned by quantmod into an ordinary data
# frame with the columns: Date, Open, High, Low, Close, Volume, Adjusted.
xts_to_data_frame <- function(x) {
  df <- data.frame(Date = index(x), coredata(x), row.names = NULL)
  # Remove the "SYMBOL." prefix quantmod puts in front of each column name
  colnames(df) <- c("Date", sub("^.*\\.", "", colnames(x)))
  df <- df[!is.na(df$Close), ]   # drop days with no closing price
  rownames(df) <- NULL
  df
}

# Reads portfolio.txt and imports the data for every symbol.
# Returns a named list holding ONE DATA FRAME PER SYMBOL, e.g. stocks$AAPL.
# If assign_globals = TRUE, each data frame is also created as its own
# variable in the workspace (AAPL_df, MSFT_df, ...), so it shows up in
# RStudio's Environment pane.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = Sys.Date() - 365 * 2,
                            to   = Sys.Date(),
                            src  = "yahoo",
                            assign_globals = TRUE,
                            env = .GlobalEnv) {
  symbols <- read_portfolio(portfolio_file)
  stocks  <- list()
  failed  <- character(0)

  for (sym in symbols) {
    message("Importing ", sym, " ...")
    df <- tryCatch(
      xts_to_data_frame(fetch_one_symbol(sym, from, to, src)),
      error = function(e) {
        message("  could not import ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) {
      failed <- c(failed, sym)
      next
    }
    stocks[[sym]] <- df
    if (assign_globals) {
      assign(paste0(make.names(sym), "_df"), df, envir = env)
    }
  }

  message("Imported ", length(stocks), " of ", length(symbols), " symbols.")
  if (length(failed) > 0) {
    warning("These symbols could not be imported: ",
            paste(failed, collapse = ", "), call. = FALSE)
  }
  stocks
}


# ---------------------------------------------------------------------
# 3. Basic statistics
# ---------------------------------------------------------------------
# Adds simple moving average columns (SMA20, SMA50, ...) to a stock
# data frame. The first (period - 1) values are NA by definition.
add_moving_averages <- function(df, periods = c(20, 50), price_col = "Close") {
  for (p in periods) {
    df[[paste0("SMA", p)]] <- if (nrow(df) >= p) {
      as.numeric(SMA(df[[price_col]], n = p))
    } else {
      NA_real_
    }
  }
  df
}

# Mode of a price series. Prices are continuous, so they are rounded to
# 'digits' decimal places before counting. If several values tie, the
# lowest one is returned (ModeCount in the output shows how often it occurs).
stat_mode <- function(x, digits = 2) {
  x <- round(x[!is.na(x)], digits)
  counts <- table(x)
  top <- names(counts)[counts == max(counts)]
  list(value = as.numeric(top[1]), count = as.integer(max(counts)))
}

# Takes ONE stock's data frame and returns a ONE-ROW data frame with:
# observations, date range, latest close, latest moving average,
# mean, median, mode, and standard deviation of the closing price.
calculate_statistics <- function(df, symbol = NA_character_,
                                 price_col = "Close", ma_period = 20) {
  if (!is.data.frame(df) || !(price_col %in% colnames(df))) {
    stop("'df' must be a data frame containing a '", price_col, "' column.")
  }
  prices <- df[[price_col]]
  prices <- prices[!is.na(prices)]

  ma_latest <- NA_real_
  if (length(prices) >= ma_period) {
    ma_latest <- tail(as.numeric(SMA(prices, n = ma_period)), 1)
  } else {
    warning(symbol, ": fewer than ", ma_period,
            " observations, moving average set to NA.", call. = FALSE)
  }

  m <- stat_mode(prices)
  avg <- mean(prices)
  sdv <- sd(prices)

  out <- data.frame(
    Symbol       = symbol,
    Observations = length(prices),
    StartDate    = min(df$Date),
    EndDate      = max(df$Date),
    LatestClose  = tail(prices, 1),
    MovingAvg    = ma_latest,
    Mean         = avg,
    Median       = median(prices),
    Mode         = m$value,
    ModeCount    = m$count,
    StdDev       = sdv,
    CV_pct       = 100 * sdv / avg,   # std dev as % of the mean (for comparing stocks)
    stringsAsFactors = FALSE
  )
  names(out)[names(out) == "MovingAvg"] <- paste0("SMA", ma_period)
  out
}

# Runs calculate_statistics() on every stock in the list returned by
# load_stock_data() and stacks the results into one summary data frame.
calculate_all_statistics <- function(stocks, price_col = "Close", ma_period = 20) {
  rows <- lapply(names(stocks), function(sym) {
    calculate_statistics(stocks[[sym]], symbol = sym,
                         price_col = price_col, ma_period = ma_period)
  })
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}
