# Assignment 5 - Technical Analysis using R (Development Phase)
BDA400 Data Science Tools and Techniques, CDI College. Author: Sodiq Yunusa

Base-R implementations (no libraries) of nine technical-analysis functions.

| File | Function |
|---|---|
| sma.R | `sma(data, period)` |
| ema.R | `ema(data, period)` |
| macd.R | `macd(data, short_period, long_period, signal_period)` (uses ema.R) |
| stdev.R | `stdev(data)` |
| linreg.R | `linreg(regressionSource, regressionLength, regressionOffset)` |
| rsi.R | `rsi(data, period)` |
| stoch_rsi.R | `stoch_rsi(data, period, k_period, d_period)` (uses rsi.R, sma.R) |
| crossover.R | `crossover(arr1, arr2)` |
| crossunder.R | `crossunder(arr1, arr2)` |
| run_tests.R | 46 automated checks |

## How to run
1. Open RStudio and set the working directory to this folder (Session > Set Working Directory > To Source File Location).
2. Run `source("run_tests.R")`. Expected last line: `ALL 46 CHECKS PASSED`.
3. To use one function: `source("sma.R"); sma(c(10,12,15,20,18,22,25,24,21), period = 3)`.

See `SodiqYunusa_BDA400_A05.docx` for the explanation of each implementation.
