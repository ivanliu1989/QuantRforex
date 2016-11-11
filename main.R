library(AutoPairTrading)
getUserTemplate2()


# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(TTR)
library(data.table)

# 1. Get Y, X and Price.Ratio ---------------------------------------------
tscale <- "2014-01-01/2016-10-01"
y = AUDUSD[tscale]
x = CADUSD[tscale]
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


# 8. Interest Rates data ----------------------------------------------------
# head(SampleUniverse)
all.bonds <- na.omit(merge(AUSYC, CANYC, USAYC))
# 8.1. Uncovered Interest Rate Parity ---------------------------------------
all.bonds$IR.AUSUSA.2 = (all.bonds$AUS2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.AUSUSA.3 = (all.bonds$AUS3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.AUSUSA.5 = (all.bonds$AUS5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.AUSUSA.10 = (all.bonds$AUS10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.CANUSA.2 = (all.bonds$CAN2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.CANUSA.3 = (all.bonds$CAN3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.CANUSA.5 = (all.bonds$CAN5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.CANUSA.10 = (all.bonds$CAN10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.AUSCAN.2 = (all.bonds$AUS2Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.AUSCAN.3 = (all.bonds$AUS3Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.AUSCAN.5 = (all.bonds$AUS5Y+100)/(all.bonds$CAN5Y+100)
all.bonds$IR.AUSCAN.10 = (all.bonds$AUS10Y+100)/(all.bonds$CAN10Y+100)

# 8.2. Calculate Forward Rate -----------------------------------------------
all.bonds$IR.AUS.10.2 = (all.bonds$AUS10Y+100)/(all.bonds$AUS2Y+100)
all.bonds$IR.AUS.10.3 = (all.bonds$AUS10Y+100)/(all.bonds$AUS3Y+100)
all.bonds$IR.AUS.10.5 = (all.bonds$AUS10Y+100)/(all.bonds$AUS5Y+100)
all.bonds$IR.CAN.10.2 = (all.bonds$CAN10Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.CAN.10.3 = (all.bonds$CAN10Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.CAN.10.5 = (all.bonds$CAN10Y+100)/(all.bonds$CAN5Y+100)

all.bonds = all.bonds[, -c(1:12)]


# 9. Quandl data -----------------------------------------------------------
library(AutoPairTrading)
QuandlConnect()

# 9.1. CFTC -----------------------------------------------------------------
calNet <- function(x,y){
  net <- (x-y)/(x+y)
  return(net)
}

CFTC.AUD <- Quandl("CFTC/TIFF_CME_AD_ALL")
CFTC.AUD <- as.xts(calNet(CFTC.AUD$`Lev Money Long Positions`,CFTC.AUD$`Lev Money Short Positions`),CFTC.AUD$Date)
CFTC.CAD <- Quandl("CFTC/TIFF_CME_CD_ALL")
CFTC.CAD <- as.xts(calNet(CFTC.CAD$`Lev Money Long Positions`,CFTC.CAD$`Lev Money Short Positions`),CFTC.CAD$Date)

all.CFTC <- na.omit(merge(CFTC.AUD, CFTC.CAD))


# 9.2. Equity Index ---------------------------------------------------------
# AU
EI.ASX <- Quandl("YAHOO/INDEX_AXJO")
# Canada
EI.TSX <- Quandl("YAHOO/INDEX_GSPTSE")
# USA
EI.USA <- Quandl("YAHOO/INDEX_GSPC")
# China
EI.SSEC <- Quandl("YAHOO/INDEX_SSEC")
# Mexico
EI.MXX <- Quandl("YAHOO/INDEX_MXX")
# Germany
EI.GDAXI <- Quandl("YAHOO/INDEX_GDAXI")
# Japan
EI.NIKKEI <- Quandl("NIKKEI/INDEX")
# South Korea
EI.KS11 <- Quandl("YAHOO/INDEX_KS11")
# France
EI.FCHI <- Quandl("YAHOO/INDEX_FCHI")
# Switzerland
EI.SSMI <- Quandl("YAHOO/INDEX_SSMI")
# India
EI.BSESN <- Quandl("YAHOO/INDEX_BSESN")
# Brazil
EI.BCB <- Quandl("BCB/7")
# Netherlands
EI.AEX <- Quandl("YAHOO/INDEX_AEX")
# Germany
EI.GDAXI <- Quandl("YAHOO/INDEX_GDAXI")
# Singapore
EI.STI <- Quandl("YAHOO/INDEX_STI")
# New Zealand
EI.NZ50 <- Quandl("YAHOO/INDEX_NZ50")
# Indonesia
EI.JKSE <- Quandl("YAHOO/INDEX_JKSE")
# Malaysia
# India
# Peru
# Thailand
# Vietnam
# UK
# Italy


all.equity.index <- merge(as.xts(EI.ASX$`Adjusted Close`, EI.ASX$Date)
                          ,as.xts(EI.TSX$`Adjusted Close`, EI.TSX$Date)
                          ,as.xts(EI.USA$`Adjusted Close`, EI.USA$Date)
                          ,as.xts(EI.SSEC$`Adjusted Close`, EI.SSEC$Date)
                          ,as.xts(EI.MXX$`Adjusted Close`, EI.MXX$Date)
                          ,as.xts(EI.GDAXI$`Adjusted Close`, EI.GDAXI$Date)
                          ,as.xts(EI.NIKKEI$`Close Price`, EI.NIKKEI$Date)
                          ,as.xts(EI.KS11$`Adjusted Close`, EI.KS11$Date)
                          ,as.xts(EI.FCHI$`Adjusted Close`, EI.FCHI$Date)
                          ,as.xts(EI.SSMI$`Adjusted Close`, EI.SSMI$Date)
                          ,as.xts(EI.BSESN$`Adjusted Close`, EI.BSESN$Date)
                          ,as.xts(EI.BCB$Value, EI.BCB$Date)
                          ,as.xts(EI.AEX$`Adjusted Close`, EI.AEX$Date)
                          ,as.xts(EI.GDAXI$`Adjusted Close`, EI.GDAXI$Date)
                          ,as.xts(EI.STI$`Adjusted Close`, EI.STI$Date)
                          ,as.xts(EI.NZ50$`Adjusted Close`, EI.NZ50$Date)
                          ,as.xts(EI.JKSE$`Adjusted Close`, EI.JKSE$Date))
names(all.equity.index) <- c("ASX", "TSX", "SP500", "SSEC", "MXX", "GDAXI",
                             "NIKKEI", "KS11", "FCHI", "SSMI", "BSESN", "BCB",
                             "AEX", "GDAXI", "STI", "NZ50", "JKSE")
all.equity.index <- na.omit(all.equity.index)



# 10. ARMIA + GARCH -------------------------------------------------------
library(lattice)
library(timeSeries)
library(rugarch)

# Create the forecasts vector to store the predictions
spReturns = diff(log(price.ratio))
windowLength = 250
foreLength = length(spReturns) - windowLength
forecasts <- vector(mode="character", length=foreLength)
for (d in 1:foreLength) {
  # Obtain the rolling window for this day
  spReturnsOffset = spReturns[(1+d):(windowLength+d)]
  # Fit the ARIMA model
  final.aic <- Inf
  final.order <- c(0,0,0)
  for (p in 0:5) for (q in 0:5) {
    if ( p == 0 && q == 0) {
      next
    }
    arimaFit = tryCatch( arima(spReturnsOffset, order=c(p, 0, q)),
                         error=function( err ) FALSE,
                         warning=function( err ) FALSE )
    if( !is.logical( arimaFit ) ) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) {
        final.aic <- current.aic
        final.order <- c(p, 0, q)
        final.arima <- arima(spReturnsOffset, order=final.order)
      }
    } else {
      next
    }
  }
  # Specify and fit the GARCH model
  spec = ugarchspec(
    variance.model=list(garchOrder=c(1,1)),
    mean.model=list(armaOrder=c(
      final.order[1], final.order[3]
    ), include.mean=T),
    distribution.model="sged"
  )
  fit = tryCatch(
    ugarchfit(
      spec, spReturnsOffset, solver = 'hybrid'
    ), error=function(e) e, warning=function(w) w
  )
  # If the GARCH model does not converge, set the direction to "long" else
  # choose the correct forecast direction based on the returns prediction
  # Output the results to the screen and the forecasts vector
  if(is(fit, "warning")) {
    forecasts[d+1] = paste(
      index(spReturnsOffset[windowLength]), 1, sep=","
    )
    print(
      paste(
        index(spReturnsOffset[windowLength]), 1, sep=","
      )
    )
  } else {
    fore = ugarchforecast(fit, n.ahead=1)
    ind = fore@forecast$seriesFor
    forecasts[d+1] = paste(
      colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","
    )
    print(paste(colnames(ind), ifelse(ind[1] < 0, -1, 1), sep=","))
  }
}

pred = strsplit(forecasts, split = ',')
pred[[1]] <- NULL
setDT(pred)
pred[2,]
prediction <- xts(x = as.numeric(pred[2, ]), order.by = as.Date(as.character(pred[1, ])))
prediction <- na.omit(merge(spReturns, prediction))
prediction$actual = ifelse(prediction$price.ratio >= 0, 1, -1)
head(prediction)
acc <- sum(prediction$prediction == prediction$actual)/nrow(prediction)



# 11. IR Support Vector Machine -------------------------------------------
target <- ifelse(spReturns >= 0, 1, -1)
names(target) <- 'target'
model_data <- na.omit(merge(target, lag(diff(log(all.bonds)),1),
                            lag(diff(log(all.bonds)),7),
                            lag(diff(log(all.bonds)),14)))
head(model_data)

breakpoint = nrow(model_data)*(2/3)
training_data = model_data[1:breakpoint,]
test_data = model_data[(breakpoint+1):nrow(model_data),]

# Use the SVM to find patterns and then make predictions ------------------
SVM = svm(as.factor(target)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
predictions = predict(SVM, training_data, type = "class", probability=FALSE)
trainingdata = data.frame(training_data, predictions)
accuracy = (sum(trainingdata$predictions == trainingdata$target)/nrow(trainingdata))*100
print(accuracy)

predictions2 = predict(SVM, test_data, type = "class")
testdata = data.frame(test_data, predictions2)
accuracy2 = (sum(testdata$predictions == testdata$target)/nrow(testdata))*100
print(accuracy2)



# 12. Equity Index Support Vector Machine ---------------------------------
target <- ifelse(spReturns >= 0, 1, -1)
names(target) <- 'target'
model_data <- na.omit(merge(target, lag(diff(log(all.equity.index)),1),
                            lag(diff(log(all.equity.index)),3),
                            lag(diff(log(all.equity.index)),7),
                            lag(diff(log(all.equity.index)),14)))
head(model_data)

breakpoint = nrow(model_data)*(2/3)
training_data = model_data[1:breakpoint,]
test_data = model_data[(breakpoint+1):nrow(model_data),]

# Use the SVM to find patterns and then make predictions ------------------
SVM = svm(as.factor(target)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
predictions = predict(SVM, training_data, type = "class", probability=FALSE)
trainingdata = data.frame(training_data, predictions)
accuracy = (sum(trainingdata$predictions == trainingdata$target)/nrow(trainingdata))*100
print(accuracy)

predictions2 = predict(SVM, test_data, type = "class")
testdata = data.frame(test_data, predictions2)
accuracy2 = (sum(testdata$predictions == testdata$target)/nrow(testdata))*100
print(accuracy2)



# 13. TIFF Support Vector Machine -----------------------------------------
target <- ifelse(spReturns >= 0, 1, -1)
names(target) <- 'target'
model_data <- na.omit(merge(target, lag(all.CFTC,1), lag(all.CFTC,2), lag(all.CFTC,3)))
head(model_data)

breakpoint = nrow(model_data)*(2/3)
training_data = model_data[1:breakpoint,]
test_data = model_data[(breakpoint+1):nrow(model_data),]

# Use the SVM to find patterns and then make predictions ------------------
SVM = svm(as.factor(target)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
predictions = predict(SVM, training_data, type = "class", probability=FALSE)
trainingdata = data.frame(training_data, predictions)
accuracy = (sum(trainingdata$predictions == trainingdata$target)/nrow(trainingdata))*100
print(accuracy)

predictions2 = predict(SVM, test_data, type = "class")
testdata = data.frame(test_data, predictions2)
accuracy2 = (sum(testdata$predictions == testdata$target)/nrow(testdata))*100
print(accuracy2)


# 14. Price lags Support Vector Machine -----------------------------------
lag1 <- lag(spReturns, 1)
lag2 <- lag(spReturns, 3)
lag3 <- lag(spReturns, 9)
lag4 <- lag(spReturns, 15)
lag5 <- lag(spReturns, 30)
lags <- na.omit(merge(lag1, lag2, lag3, lag4, lag5))
head(lags)
target <- ifelse(spReturns >= 0, 1, -1)
names(target) <- 'target'
model_data <- na.omit(merge(target, lags))
head(model_data)

breakpoint = nrow(model_data)*(2/3)
training_data = model_data[1:breakpoint,]
test_data = model_data[(breakpoint+1):nrow(model_data),]

# Use the SVM to find patterns and then make predictions ------------------
SVM = svm(as.factor(target)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
predictions = predict(SVM, training_data, type = "class", probability=FALSE)
trainingdata = data.frame(training_data, predictions)
accuracy = (sum(trainingdata$predictions == trainingdata$target)/nrow(trainingdata))*100
print(accuracy)

predictions2 = predict(SVM, test_data, type = "class")
testdata = data.frame(test_data, predictions2)
accuracy2 = (sum(testdata$predictions == testdata$target)/nrow(testdata))*100
print(accuracy2)

ggplot(trainingdata, aes(x=price.ratio, y=price.ratio.1)) +
  stat_density2d(geom="contour",aes(color=predictions)) +
  labs(title="SVM Predictions", x="lag1", y="lag2", color="Training Predictions")
