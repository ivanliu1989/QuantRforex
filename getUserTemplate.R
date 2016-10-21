# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(AutoPairTrading)


# 1. Get Y, X and Price.Ratio ---------------------------------------------
# tscale <- "2014-01-01/2016-10-01"
# y = AUDUSD[tscale]
# x = CADUSD[tscale]
# price.ratio <- getPriceRatio(y, x, FALSE)
# names(price.ratio) = "price.ratio"


# 1.2 Extract data from interactive brokers -------------------------------
CADUSD <- reqHistoryFX(duration = "10 Y", barsize = "1 day", Cur1 = "USD", Cur2 = "CAD")$CleanData
CADUSD <- 1/CADUSD
AUDUSD <- reqHistoryFX(duration = "10 Y", barsize = "1 day", Cur1 = "AUD", Cur2 = "USD")$CleanData
NZDUSD <- reqHistoryFX(duration = "10 Y", barsize = "1 day", Cur1 = "NZD", Cur2 = "USD")$CleanData

tscale <- "2014-01-01/2016-07-22"
y.series = AUDUSD[tscale]
x.series = CADUSD[tscale]
y = y.series$Close.price
x = x.series$Close.price
price.ratio <- getPriceRatio(y, x, FALSE)
names(price.ratio) = "price.ratio"

# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = "drift", lags = 1); cat(paste0("P-value: ", adf.y$signif[[1]]))
adf.x <- AugmentedDickeyFullerTest(x, type = "drift", lags = 1); cat(paste0("P-value: ", adf.x$signif[[1]]))
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = "drift", lags = 1); cat(paste0("P-value: ", adf.y.ret$signif[[1]]))
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = "drift", lags = 1); cat(paste0("P-value: ", adf.x.ret$signif[[1]]))


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = "drift", lags = 1); cat(paste0("P-value: ", adf.ratio$signif[[1]]))
jc.test <- JohansenCointegrationTest(merge(y,x), type = "trace", ecdet = "none", K = 2); cat(paste0("P-value: ", jc.test$signif[[1]]))


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round; cat(paste0("Half-Life: ", half.life))


# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, half.life); cat(paste0("Hurse Exponent: ", mean(hurst.test$hurstKY, na.rm = T)))


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])
hedgeRatio


# 8. Preparing the Universe data ------------------------------------------
head(SampleUniverse)
index(y.series) <- as.Date(index(y.series))
index(x.series) <- as.Date(index(x.series))
index(price.ratio) <- as.Date(index(price.ratio))

Universe <- merge(price.ratio, y.series, x.series, SampleUniverse[,-c(1:3)])
names(Universe) <- c("price.ratio", "y.close", "y.bid", "y.ask", "x.close", "x.bid", "x.ask", names(SampleUniverse[,-c(1:3)]))
Universe <- na.omit(Universe)

# 9. Back Testing ---------------------------------------------------------
context <- InitializeContext(Universe$y.close, Universe$x.close, capital = 1e6, window = 20,
                             lookback = 252, brokerage = 0.001, stoploss = 0.1, half.life = half.life)
dt.summary <- BackTestingRealTime(context, Universe, nEval = 350)
dt.summary <- BackTestingRealTimeBenchmark(context, Universe, nEval = 350)


# 10. Performance Analytics -----------------------------------------------
basic.report <- performanceReport(dt.summary)
performanceEvaluationEmail(basic.report, c("ivan.liuyanfeng@gmail.com"), message = "Machine Learning and Mean Reversion - Bid & Ask")


# 11. Searching Good Integrated Pairs -------------------------------------
data("sp500")
datasets <- sp500[,c(50:80)]
searchCointegratedPairs(datasets, path = "GoodIntegratedPairs.pdf",
                        to = c("ivan.liuyanfeng@gmail.com", "ivan@growingdata.com.au"),
                        testPeriod = 63, trainPeriod = 252)
