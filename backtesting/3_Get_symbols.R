source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
print(basic_symbols())
symbols <- basic_symbols()


# 1. Yahoo ----------------------------------------------------------------
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
head(IWM)
summary(IWM)
rm(list=basic_symbols())


# 2. Google ---------------------------------------------------------------
getSymbols(Symbols = symbols,
           src = "google",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)
head(IWM)
summary(IWM)
rm(list=basic_symbols())


# 3. FRED -----------------------------------------------------------------
getSymbols(Symbols = "DGS10", src = "FRED")
chartSeries(DGS10)
rm(DGS10)

