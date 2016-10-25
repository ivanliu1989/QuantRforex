library(quantmod)
library(TTR)
library(e1071)
library(ggplot2)
library(AutoPairTrading)


# data = getFX("EUR/USD",from="2005-01-01")
# data = AUDUSD
# macd_data <- MACD(data, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)



# con = gzcon(file('C:/Users/sky_x/Desktop/sit.gz', 'rb'))
# source(con)
# close(con)
# plota.test()

reqHistoryFX2 <- function(tws = NULL, duration = "10 Y", barsize = "1 day", Cur1 = "USD", Cur2 = "CAD"){
  library(IBrokers)
  if(is.null(tws)){
    tws <- twsConnect(clientId = 999)
  }
  
  ccy <- reqContractDetails(tws, twsCurrency(Cur1, Cur2))[[1]]$contract
  
  BIDASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                              duration = duration, useRTH = "1", whatToShow='MIDPOINT')
  
  BID <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                           duration = duration, useRTH = "1", whatToShow='BID')
  
  ASK <- reqHistoricalData(conn = tws, Contract = ccy, barSize = barsize,
                           duration = duration, useRTH = "1", whatToShow='ASK')
  
  CleanData <- merge(BIDASK[,4], BID[,4], ASK[, 4])
  colnames(CleanData) <- c("Close.price", "Bid.price", "Ask.price")
  
  res <- list(BIDASK = BIDASK,
              BID = BID,
              ASK = ASK,
              CleanData = CleanData)
  
  if(is.null(tws)){
    twsDisconnect(tws)
  }
  
  return(res)
}



# Read data and use indicators --------------------------------------------
# Using MACD and Parabolic SAR as indicators ------------------------------
data = reqHistoryFX2(tws = NULL, duration = "6 Y", barsize = "1 hour", Cur1 = "EUR", Cur2 = "USD")
head(data$BIDASK)
data = data$BIDASK[,1:4]
colnames(data) <- c("Open", "High", "Low", "Close")
head(data)

macd_data = MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)
sar = SAR(data[, c("High", "Low")], accel = c(0.02, 0.2))
trend = data$Close - sar

price = data$Close - data$Open
class = ifelse(price > 0, "UP", "DOWN")


# Merging the indicators and creating training and testing dataset --------
macd_data = data.frame(macd_data)
histogram = macd_data$macd - macd_data$signal
histogram = c(NA, head(histogram, -1)) # creating a lag to avoid look-ahead bias

trend = c(NA, head(trend, -1))

model_data = data.frame(class, trend, histogram)
model_data = model_data[-c(1:34),]

breakpoint = nrow(model_data)*(2/3)
training_data = model_data[1:breakpoint,]
test_data = model_data[(breakpoint+1):nrow(model_data),]


# Use the SVM to find patterns and then make predictions ------------------
SVM = svm(Close~trend+histogram, data=training_data, kernel="radial",cost=1,gamma=0.5)
predictions = predict(SVM, training_data, type = "class")
trainingdata = data.frame(training_data, predictions)
accuracy = (sum(trainingdata$predictions == trainingdata$Close)/nrow(trainingdata))*100
print(accuracy)

predictions2 = predict(SVM, test_data, type = "class")
testdata = data.frame(test_data, predictions2)
accuracy2 = (sum(testdata$predictions == testdata$Close)/nrow(testdata))*100
print(accuracy2)


# Plot the pattern using the ggplot package -------------------------------
ggplot(trainingdata, aes(x=histogram, y=trend)) +
  stat_density2d(geom="contour",aes(color=predictions)) +
  labs(title="SVM Predictions", x="MACD", y="Price - SAR", color="Training Predictions")
