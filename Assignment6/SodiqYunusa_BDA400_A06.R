# =============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase
# Author : Sodiq Yunusa
# Purpose: Interactive R Shiny portfolio dashboard that fetches stock data,
#          draws line / candlestick / area charts, overlays technical
#          indicators (Moving Averages, RSI, MACD), generates Buy / Sell / Hold
#          trading signals and annotates the chart with them.
#
# HOW TO RUN
#   * RStudio : open this file and click the "Run App" button, or
#   * Console : shiny::runApp("SodiqYunusa_BDA400_A06.R")
#
# FILE LAYOUT (one section per task in the assignment brief)
#   SECTION 1 - Data collection and setup
#   SECTION 2 - Visualizing stock data (UI skeleton, widgets, base chart)
#   SECTION 3 - Overlay technical indicators (with on/off toggles)
#   SECTION 4 - Trading rules and chart annotations
#   SECTION 5 - Server logic (puts sections 1-4 together)
# =============================================================================


# =============================================================================
# SECTION 1 - DATA COLLECTION AND SETUP
# =============================================================================

# 1.1 Install (only if missing) and load the necessary packages ---------------
required_pkgs <- c("shiny", "ggplot2", "quantmod")
missing_pkgs  <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

suppressPackageStartupMessages({
  library(shiny)      # web application framework
  library(ggplot2)    # charts
  library(quantmod)   # getSymbols(); also loads xts, zoo and TTR (SMA, EMA, RSI, MACD)
})

# 1.2 Data source -------------------------------------------------------------
# Primary source : Yahoo Finance (through quantmod::getSymbols)
# Fallback source: Stooq (daily CSV download) - used automatically if Yahoo fails
DATA_SOURCES <- c("Auto (Yahoo, then Stooq)" = "auto",
                  "Yahoo Finance"            = "yahoo",
                  "Stooq (CSV)"              = "stooq")

# Extra history fetched BEFORE the chosen start date so that long indicators
# (e.g. 50-period moving average) already have values on the first visible bar.
WARMUP_DAYS <- 365 * 6

# 1.3 Portfolio symbols (from Assignment 2: portfolio.txt) ---------------------
# One symbol per line. If the file is missing, a default portfolio is used.
load_portfolio <- function(path = "portfolio.txt") {
  default <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM", "KO")
  if (!file.exists(path)) return(default)
  syms <- trimws(readLines(path, warn = FALSE))
  syms <- toupper(syms[nzchar(syms) & !grepl("^#", syms)])
  if (length(syms) == 0) default else unique(syms)
}
portfolio_symbols <- load_portfolio()

# 1.4 Fetch from Yahoo Finance --------------------------------------------------
fetch_yahoo <- function(symbol, from, to) {
  raw <- getSymbols(symbol, src = "yahoo", from = from, to = to,
                    auto.assign = FALSE)
  # getSymbols names columns "AAPL.Open", "AAPL.High", ... -> keep the last word
  colnames(raw) <- sub("^.*\\.", "", colnames(raw))
  raw[, c("Open", "High", "Low", "Close", "Volume")]
}

# 1.5 Fetch from Stooq (fallback source) --------------------------------------
fetch_stooq <- function(symbol, from, to) {
  url <- sprintf("https://stooq.com/q/d/l/?s=%s.us&d1=%s&d2=%s&i=d",
                 tolower(symbol), format(from, "%Y%m%d"), format(to, "%Y%m%d"))
  df <- read.csv(url, stringsAsFactors = FALSE)
  needed <- c("Date", "Open", "High", "Low", "Close", "Volume")
  if (!all(needed %in% names(df)) || nrow(df) == 0)
    stop("Stooq returned no usable data for ", symbol)
  xts(df[, c("Open", "High", "Low", "Close", "Volume")],
      order.by = as.Date(df$Date))
}

# 1.6 Clean the data (missing values, ordering, numeric types) ----------------
clean_ohlcv <- function(x) {
  x <- x[!is.na(index(x)), ]
  x <- x[complete.cases(x[, c("Open", "High", "Low", "Close")]), ]  # drop rows with NA prices
  x[is.na(x[, "Volume"]), "Volume"] <- 0
  storage.mode(x) <- "numeric"
  x[!duplicated(index(x)), ]
}

# 1.7 Master fetch function with error handling and source fallback -----------
# Returns list(data = <xts OHLCV>, source = <name of source used>)
get_stock_data <- function(symbol, start_date, end_date, source = "auto") {
  from <- as.Date(start_date) - WARMUP_DAYS
  to   <- as.Date(end_date) + 1               # Yahoo's 'to' date is exclusive
  tries <- switch(source,
                  auto  = list(Yahoo = fetch_yahoo, Stooq = fetch_stooq),
                  yahoo = list(Yahoo = fetch_yahoo),
                  stooq = list(Stooq = fetch_stooq))
  errors <- character(0)
  for (nm in names(tries)) {
    res <- tryCatch({
      x <- clean_ohlcv(tries[[nm]](symbol, from, to))
      if (nrow(x) < 2) stop("fewer than 2 rows returned")
      list(data = x, source = nm)
    }, error = function(e) {
      errors <<- c(errors, paste0(nm, ": ", conditionMessage(e)))
      NULL
    })
    if (!is.null(res)) return(res)
  }
  stop("Could not download data for '", symbol, "'. Check the ticker symbol and ",
       "your internet connection. (", paste(errors, collapse = " | "), ")")
}


# =============================================================================
# SECTION 2 - VISUALIZING STOCK DATA
# =============================================================================

# 2.1 Time frame conversion: Daily / Weekly / Monthly -------------------------
resample_ohlc <- function(x, time_frame) {
  if (time_frame == "Daily") return(x)
  period <- if (time_frame == "Weekly") "weeks" else "months"
  out <- to.period(x, period = period)          # Open of first, High max, Low min, Close of last
  colnames(out) <- c("Open", "High", "Low", "Close", "Volume")
  out
}

# 2.2 Colours used across the chart -------------------------------------------
COL_UP   <- "#16a34a"   # green
COL_DOWN <- "#dc2626"   # red
COL_LINE <- "#1f4e79"   # price line / area

# 2.3 Base price chart: line, candlestick or area -----------------------------
# 'with_panel' tags a data frame with the facet panel it belongs to.
# The price, RSI and MACD panels are stacked in ONE ggplot (facet_grid), so
# every indicator is a layer of the same chart and shares the same x-axis.
add_price_layer <- function(p, df, chart_type, with_panel) {
  pd <- with_panel(df, "Price")
  if (chart_type == "Line graph") {
    p <- p + geom_line(data = pd, aes(x = Date, y = Close),
                       colour = COL_LINE, linewidth = 0.7)
  } else if (chart_type == "Area chart") {
    pd$Floor <- min(pd$Close, na.rm = TRUE) * 0.97     # base of the shaded area
    p <- p +
      geom_ribbon(data = pd, aes(x = Date, ymin = Floor, ymax = Close),
                  fill = COL_LINE, alpha = 0.25) +
      geom_line(data = pd, aes(x = Date, y = Close),
                colour = COL_LINE, linewidth = 0.7)
  } else {                                              # Candlestick
    step <- median(diff(as.numeric(pd$Date)))           # bar spacing in days
    half <- max(step, 1) * 0.35
    pd$Dir <- ifelse(pd$Close >= pd$Open, "Up", "Down")
    p <- p +
      geom_linerange(data = pd, aes(x = Date, ymin = Low, ymax = High),
                     colour = "grey30", linewidth = 0.3) +
      geom_rect(data = pd,
                aes(xmin = Date - half, xmax = Date + half,
                    ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                    fill = Dir),
                colour = "grey30", linewidth = 0.2)
  }
  p
}


# =============================================================================
# SECTION 3 - OVERLAY TECHNICAL INDICATORS
# =============================================================================

# 3.1 Calculate every indicator once (TTR functions) --------------------------
# p is a list of parameters taken from the sidebar widgets.
# tryCatch makes sure a too-short series gives NA values instead of an error.
add_indicators <- function(x, p) {
  n  <- nrow(x)
  cl <- Cl(x)
  na_vec <- rep(NA_real_, n)
  ma_fun <- if (p$ma_type == "EMA") EMA else SMA

  ma_s <- tryCatch(as.numeric(ma_fun(cl, n = p$ma_short)), error = function(e) na_vec)
  ma_l <- tryCatch(as.numeric(ma_fun(cl, n = p$ma_long)),  error = function(e) na_vec)
  rsi  <- tryCatch(as.numeric(RSI(cl, n = p$rsi_period)),  error = function(e) na_vec)
  mac  <- tryCatch(MACD(cl, nFast = p$macd_fast, nSlow = p$macd_slow,
                        nSig = p$macd_signal, maType = "EMA"),
                   error = function(e) NULL)
  macd_line <- if (is.null(mac)) na_vec else as.numeric(mac[, "macd"])
  macd_sig  <- if (is.null(mac)) na_vec else as.numeric(mac[, "signal"])

  data.frame(Date = as.Date(index(x)),
             Open = as.numeric(Op(x)), High = as.numeric(Hi(x)),
             Low  = as.numeric(Lo(x)), Close = as.numeric(cl),
             Volume = as.numeric(Vo(x)),
             MA_Short = ma_s, MA_Long = ma_l, RSI = rsi,
             MACD = macd_line, MACD_Signal = macd_sig,
             MACD_Hist = macd_line - macd_sig)
}

# 3.2 Add the indicators as extra layers (only the ones that are switched on) --
add_indicator_layers <- function(p, df, indicators, prm, panels, with_panel) {
  lab_s <- sprintf("%s %d", prm$ma_type, prm$ma_short)
  lab_l <- sprintf("%s %d", prm$ma_type, prm$ma_long)
  lab_r <- sprintf("RSI (%d)", prm$rsi_period)
  lab_m <- sprintf("MACD (%d,%d,%d)", prm$macd_fast, prm$macd_slow, prm$macd_signal)
  step  <- max(median(diff(as.numeric(df$Date))), 1)

  # fixed colour for every named line so legends stay consistent
  cols <- c("#f59e0b", "#8b5cf6", "#0ea5e9", "#0d9488", "#e11d48")
  names(cols) <- c(lab_s, lab_l, lab_r, lab_m, "Signal line")
  active <- character(0)

  ## Moving averages: drawn on top of the price panel ------------------------
  if ("Moving Averages" %in% indicators) {
    ma <- rbind(data.frame(Date = df$Date, value = df$MA_Short, line = lab_s),
                data.frame(Date = df$Date, value = df$MA_Long,  line = lab_l))
    ma <- with_panel(ma[!is.na(ma$value), ], "Price")
    p  <- p + geom_line(data = ma, aes(x = Date, y = value, colour = line),
                        linewidth = 0.8)
    active <- c(active, lab_s, lab_l)
  }

  ## RSI: own panel under the price with overbought / oversold guide lines ---
  if ("RSI" %in% indicators) {
    rs <- with_panel(data.frame(Date = df$Date, value = df$RSI, line = lab_r), lab_r)
    rs <- rs[!is.na(rs$value), ]
    guides_df <- with_panel(data.frame(y = c(prm$rsi_low, prm$rsi_high)), lab_r)
    range_df  <- with_panel(data.frame(Date = df$Date[1], y = c(0, 100)), lab_r)
    p <- p +
      geom_blank(data = range_df, aes(x = Date, y = y)) +        # keeps axis at 0-100
      geom_hline(data = guides_df, aes(yintercept = y),
                 linetype = "dashed", colour = "grey50") +
      geom_line(data = rs, aes(x = Date, y = value, colour = line), linewidth = 0.7)
    active <- c(active, lab_r)
  }

  ## MACD: own panel with MACD line, signal line and histogram ---------------
  if ("MACD" %in% indicators) {
    hist_df <- with_panel(data.frame(Date = df$Date, value = df$MACD_Hist), lab_m)
    hist_df <- hist_df[!is.na(hist_df$value), ]
    hist_df$Dir <- ifelse(hist_df$value >= 0, "Up", "Down")
    ml <- with_panel(data.frame(Date = df$Date, value = df$MACD, line = lab_m), lab_m)
    sl <- with_panel(data.frame(Date = df$Date, value = df$MACD_Signal,
                                line = "Signal line"), lab_m)
    p <- p +
      geom_col(data = hist_df, aes(x = Date, y = value, fill = Dir),
               width = step * 0.7, alpha = 0.6) +
      geom_hline(data = with_panel(data.frame(y = 0), lab_m),
                 aes(yintercept = y), colour = "grey50") +
      geom_line(data = ml[!is.na(ml$value), ],
                aes(x = Date, y = value, colour = line), linewidth = 0.7) +
      geom_line(data = sl[!is.na(sl$value), ],
                aes(x = Date, y = value, colour = line), linewidth = 0.7)
    active <- c(active, lab_m, "Signal line")
  }

  p + scale_colour_manual(values = cols, breaks = active, name = NULL)
}


# =============================================================================
# SECTION 4 - TRADING RULES AND ANNOTATIONS
# =============================================================================

# 4.1 Crossover detection ------------------------------------------------------
# TRUE only on the bar where series 'a' moves from <= 'b' to > 'b'.
# (The brief's ifelse(short > long, ...) labels EVERY bar "Buy" or "Sell" while
#  the short MA stays above/below the long MA; a real crossover rule only
#  signals on the bar where the cross happens, so we detect that explicitly.)
cross_up <- function(a, b) {
  pa <- c(NA, head(a, -1)); pb <- c(NA, head(b, -1))
  ok <- !is.na(a) & !is.na(b) & !is.na(pa) & !is.na(pb)
  ok & a > b & pa <= pb
}
cross_down <- function(a, b) {
  pa <- c(NA, head(a, -1)); pb <- c(NA, head(b, -1))
  ok <- !is.na(a) & !is.na(b) & !is.na(pa) & !is.na(pb)
  ok & a < b & pa >= pb
}

# 4.2 Trading rules --------------------------------------------------------------
TRADING_RULES <- c("Moving Average Crossover",
                   "MA Crossover + RSI filter",
                   "MACD Signal-Line Crossover",
                   "RSI Overbought / Oversold")

# Returns a character vector of "Buy" / "Sell" / "Hold" - one value per bar.
generate_signals <- function(df, rule, prm) {
  n <- nrow(df)
  buy <- sell <- rep(FALSE, n)
  rsi_ok_buy  <- !is.na(df$RSI) & df$RSI < prm$rsi_high   # not overbought
  rsi_ok_sell <- !is.na(df$RSI) & df$RSI > prm$rsi_low    # not oversold

  if (rule == "Moving Average Crossover") {
    # short MA crosses above long MA -> Buy ; crosses below -> Sell ; else Hold
    buy  <- cross_up(df$MA_Short, df$MA_Long)
    sell <- cross_down(df$MA_Short, df$MA_Long)
  } else if (rule == "MA Crossover + RSI filter") {
    # same crossover, but ignore Buy when RSI is overbought and Sell when oversold
    buy  <- cross_up(df$MA_Short, df$MA_Long)   & rsi_ok_buy
    sell <- cross_down(df$MA_Short, df$MA_Long) & rsi_ok_sell
  } else if (rule == "MACD Signal-Line Crossover") {
    buy  <- cross_up(df$MACD, df$MACD_Signal)
    sell <- cross_down(df$MACD, df$MACD_Signal)
  } else if (rule == "RSI Overbought / Oversold") {
    # Buy when RSI climbs back above the oversold level, Sell when it drops
    # back below the overbought level
    buy  <- cross_up(df$RSI,  rep(prm$rsi_low,  n))
    sell <- cross_down(df$RSI, rep(prm$rsi_high, n))
  }
  ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
}

# 4.3 Annotate the chart with the signals --------------------------------------
add_signal_annotations <- function(p, df, chart_type, show_hold, with_panel) {
  rng <- diff(range(c(df$Low, df$High), na.rm = TRUE))
  off <- rng * 0.03
  candle <- chart_type == "Candlestick"

  buys  <- with_panel(df[df$Signal == "Buy", ],  "Price")
  sells <- with_panel(df[df$Signal == "Sell", ], "Price")
  buys$y  <- (if (candle) buys$Low   else buys$Close) - off   # marker below the bar
  sells$y <- (if (candle) sells$High else sells$Close) + off  # marker above the bar

  if (show_hold) {
    holds <- with_panel(df[df$Signal == "Hold", ], "Price")
    p <- p + geom_text(data = holds, aes(x = Date, y = Close, label = Signal),
                       vjust = 1.5, colour = "blue", size = 2.3, check_overlap = TRUE)
  }
  p +
    geom_point(data = buys,  aes(x = Date, y = y), shape = 24, size = 3.6,
               fill = COL_UP,   colour = "#14532d") +
    geom_text(data = buys,   aes(x = Date, y = y, label = Signal), vjust = 1.8,
              colour = "#14532d", fontface = "bold", size = 3.2) +
    geom_point(data = sells, aes(x = Date, y = y), shape = 25, size = 3.6,
               fill = COL_DOWN, colour = "#7f1d1d") +
    geom_text(data = sells,  aes(x = Date, y = y, label = Signal), vjust = -1,
              colour = "#7f1d1d", fontface = "bold", size = 3.2)
}


# =============================================================================
# SECTION 5 - ASSEMBLE THE CHART, UI AND SERVER
# =============================================================================

# 5.1 Build the complete chart (price + overlays + annotations) ----------------
build_chart <- function(df, chart_type, indicators, prm, rule,
                        show_hold, symbol, time_frame) {
  lab_r <- sprintf("RSI (%d)", prm$rsi_period)
  lab_m <- sprintf("MACD (%d,%d,%d)", prm$macd_fast, prm$macd_slow, prm$macd_signal)
  panels <- c("Price",
              if ("RSI"  %in% indicators) lab_r,
              if ("MACD" %in% indicators) lab_m)

  with_panel <- function(d, name) {
    d$panel <- factor(rep(name, nrow(d)), levels = panels)
    d
  }

  p <- ggplot() +
    facet_grid(panel ~ ., scales = "free_y") +
    scale_fill_manual(values = c(Up = COL_UP, Down = COL_DOWN), guide = "none")

  p <- add_price_layer(p, df, chart_type, with_panel)                     # Section 2
  p <- add_indicator_layers(p, df, indicators, prm, panels, with_panel)   # Section 3
  p <- add_signal_annotations(p, df, chart_type, show_hold, with_panel)   # Section 4

  p + scale_x_date(date_labels = "%b %Y") +
    labs(title = sprintf("%s - %s (%s)", symbol, chart_type, time_frame),
         subtitle = paste("Trading rule:", rule), x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold"))
}

# Make the price panel taller than the RSI / MACD panels
draw_chart <- function(p) {
  g <- ggplotGrob(p)
  rows <- sort(g$layout$t[grepl("^panel", g$layout$name)])
  if (length(rows) > 1)
    g$heights[rows] <- grid::unit(c(3, rep(1, length(rows) - 1)), "null")
  grid::grid.newpage()
  grid::grid.draw(g)
}

# 5.2 User interface (Shiny skeleton + interactive widgets) --------------------
ui <- fluidPage(
  titlePanel("Portfolio Technical Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data"),
      selectizeInput("symbol", "Stock symbol (pick or type your own):",
                     choices = portfolio_symbols, selected = portfolio_symbols[1],
                     options = list(create = TRUE)),
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01", end = "2023-07-01", max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("data_source", "Data source:", choices = DATA_SOURCES),
      actionButton("refresh", "Reload data", icon = icon("rotate")),
      hr(),
      h4("Chart"),
      radioButtons("chart_type", "Chart type:",
                   choices = c("Line graph", "Candlestick", "Area chart"),
                   selected = "Candlestick", inline = TRUE),
      checkboxGroupInput("technical_indicators", "Technical indicators (overlay on/off):",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = "Moving Averages"),
      conditionalPanel(
        "input.technical_indicators.indexOf('Moving Averages') > -1",
        selectInput("ma_type", "Moving average type:", c("SMA", "EMA")),
        fluidRow(column(6, numericInput("ma_short", "Short MA:", 20, min = 2, max = 200)),
                 column(6, numericInput("ma_long",  "Long MA:",  50, min = 3, max = 400)))),
      conditionalPanel(
        "input.technical_indicators.indexOf('RSI') > -1",
        numericInput("rsi_period", "RSI period:", 14, min = 2, max = 100),
        fluidRow(column(6, numericInput("rsi_low",  "Oversold:",   30, min = 1,  max = 49)),
                 column(6, numericInput("rsi_high", "Overbought:", 70, min = 51, max = 99)))),
      conditionalPanel(
        "input.technical_indicators.indexOf('MACD') > -1",
        fluidRow(column(4, numericInput("macd_fast",   "Fast:",   12, min = 2, max = 100)),
                 column(4, numericInput("macd_slow",   "Slow:",   26, min = 3, max = 200)),
                 column(4, numericInput("macd_signal", "Signal:",  9, min = 2, max = 100)))),
      hr(),
      h4("Trading rule"),
      selectInput("trading_rule", "Rule used for Buy / Sell / Hold:", TRADING_RULES),
      checkboxInput("show_hold", "Also label Hold bars", FALSE),
      downloadButton("download_signals", "Download data + signals (CSV)")
    ),
    mainPanel(
      width = 9,
      uiOutput("summary"),
      plotOutput("stock_chart", height = "640px"),
      h4("Buy / Sell signals in the selected period"),
      tableOutput("signal_table")
    )
  )
)

# 5.3 Server logic --------------------------------------------------------------
server <- function(input, output, session) {

  # ---- collect and validate all indicator parameters -------------------------
  params <- reactive({
    ints <- c("ma_short", "ma_long", "rsi_period", "rsi_low", "rsi_high",
              "macd_fast", "macd_slow", "macd_signal")
    validate(need(all(vapply(ints, function(i) isTRUE(!is.na(input[[i]])), logical(1))),
                  "Please fill in every indicator parameter with a number."))
    validate(need(input$ma_short < input$ma_long,
                  "Short MA period must be smaller than the long MA period."),
             need(input$macd_fast < input$macd_slow,
                  "MACD fast period must be smaller than the slow period."),
             need(input$rsi_low < input$rsi_high,
                  "RSI oversold level must be below the overbought level."))
    list(ma_type = input$ma_type,
         ma_short = as.integer(input$ma_short), ma_long = as.integer(input$ma_long),
         rsi_period = as.integer(input$rsi_period),
         rsi_low = input$rsi_low, rsi_high = input$rsi_high,
         macd_fast = as.integer(input$macd_fast), macd_slow = as.integer(input$macd_slow),
         macd_signal = as.integer(input$macd_signal))
  })

  # ---- download the stock data (only when symbol / dates / source change) ----
  stock_data <- reactive({
    input$refresh                                   # lets the user force a reload
    symbol <- toupper(trimws(input$symbol))
    validate(need(grepl("^[A-Z0-9.^=-]{1,12}$", symbol), "Enter a valid ticker symbol."),
             need(length(input$date_range) == 2 && !anyNA(input$date_range),
                  "Select a start and end date."),
             need(input$date_range[1] < input$date_range[2],
                  "The start date must be before the end date."))
    res <- tryCatch(
      withProgress(message = paste("Downloading", symbol, "..."), value = 0.5,
                   get_stock_data(symbol, input$date_range[1], input$date_range[2],
                                  input$data_source)),
      error = function(e) e)
    validate(need(!inherits(res, "error"), if (inherits(res, "error")) conditionMessage(res) else ""))
    if (res$source != "Yahoo" && input$data_source == "auto")
      showNotification(paste("Yahoo Finance failed - using", res$source, "instead."),
                       type = "warning")
    res
  })

  # ---- resample, add indicators, create signals, keep selected dates ---------
  analysis <- reactive({
    prm <- params()
    x   <- resample_ohlc(stock_data()$data, input$time_frame)
    df  <- add_indicators(x, prm)
    df$Signal <- generate_signals(df, input$trading_rule, prm)   # signals use full history
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 2,
                  "Not enough bars in this date range / time frame - widen the range."))
    df
  })

  # ---- main chart --------------------------------------------------------------
  output$stock_chart <- renderPlot({
    df <- analysis()
    p <- build_chart(df, input$chart_type, input$technical_indicators, params(),
                     input$trading_rule, input$show_hold,
                     toupper(trimws(input$symbol)), input$time_frame)
    draw_chart(p)
  }, res = 96)

  # ---- summary tiles -------------------------------------------------------------
  output$summary <- renderUI({
    df <- analysis()
    chg <- (tail(df$Close, 1) / df$Close[1] - 1) * 100
    tile <- function(label, value, colour = "#111827")
      column(3, div(style = "background:#f3f4f6;border-radius:8px;padding:8px 12px;margin-bottom:10px;",
                    div(style = "font-size:12px;color:#6b7280;", label),
                    div(style = paste0("font-size:20px;font-weight:600;color:", colour, ";"), value)))
    fluidRow(
      tile("Last close", sprintf("%.2f", tail(df$Close, 1))),
      tile("Change over period", sprintf("%+.1f%%", chg), if (chg >= 0) COL_UP else COL_DOWN),
      tile("Buy signals",  sum(df$Signal == "Buy"),  COL_UP),
      tile("Sell signals", sum(df$Signal == "Sell"), COL_DOWN))
  })

  # ---- signal table -----------------------------------------------------------------
  output$signal_table <- renderTable({
    df  <- analysis()
    sig <- df[df$Signal != "Hold", c("Date", "Close", "Signal")]
    if (nrow(sig) == 0)
      return(data.frame(Message = "No Buy/Sell signals for this rule in the selected period."))
    sig$Date  <- format(sig$Date, "%Y-%m-%d")
    sig$Close <- sprintf("%.2f", sig$Close)
    sig
  }, striped = TRUE, spacing = "s")

  # ---- CSV download ---------------------------------------------------------------------
  output$download_signals <- downloadHandler(
    filename = function() sprintf("%s_%s_signals.csv", toupper(trimws(input$symbol)),
                                  input$time_frame),
    content  = function(file) write.csv(analysis(), file, row.names = FALSE))
}

# 5.4 Launch the app ----------------------------------------------------------------------
shinyApp(ui, server)
