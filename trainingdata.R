rm(list = ls()); gc()
library(AutoPairTrading)
ls()
load("data/modelingRawData.RData")
tscale <- "2014-01-01/2016-07-22"


# 1. Calculate return -----------------------------------------------------
all.CFTC.ret <- ROC(all.CFTC, n = 1, type = "discrete")
names(all.CFTC.ret) <- paste0(names(all.CFTC.ret),".ret")
all.equity.index.ret <- ROC(all.equity.index, n = 1, type = "discrete")
names(all.equity.index) <- paste0("EI.", names(all.equity.index))
names(all.equity.index.ret) <- paste0(names(all.equity.index), ".ret")
all.bonds.ret <- ROC(all.bonds, n = 1, type = "discrete")
names(all.bonds.ret) <- paste0(names(all.bonds), ".ret")


# 2. Time scale' ----------------------------------------------------------
all.bonds <- all.bonds[tscale]
all.equity.index <- all.equity.index[tscale]
all.CFTC <- all.CFTC[tscale]
AUDUSD <- AUDUSD[tscale]
CADUSD <- CADUSD[tscale]
all.CFTC.ret <- all.CFTC.ret[tscale]
all.equity.index.ret <- all.equity.index.ret[tscale]

price.ratio <- AutoPairTrading::getPriceRatio(AUDUSD, CADUSD, log = F)
price.ratio <- merge(price.ratio, AUDUSD, CADUSD)



# 3. Merge ----------------------------------------------------------------
datasets <- merge(price.ratio, all.CFTC, all.x = T)
datasets$all.x <- NULL
datasets <- merge(datasets, all.CFTC.ret, all.x = T)
datasets$all.x <- NULL
datasets <- merge(datasets, all.equity.index, all.equity.index.ret, all.bonds, all.bonds.ret)


datasets[,4] <- fillMissingData(datasets[,4])  
datasets[,5] <- fillMissingData(datasets[,5])  
datasets[,6] <- fillMissingData(datasets[,6])  
datasets[,7] <- fillMissingData(datasets[,7])  

datasets <- na.omit(datasets)
head(datasets)
dim(datasets)
save(datasets, file = "data/trainingData.RData")





# model -------------------------------------------------------------------
rm(list = ls()); gc()
library(AutoPairTrading)
ls()
load("data/modelingRawData.RData")
# all.CFTC.ret <- ROC(all.CFTC, n = 1, type = "discrete")
# names(all.CFTC.ret) <- paste0(names(all.CFTC.ret),".ret")
price.ratio <- AutoPairTrading::getPriceRatio(AUDUSD, CADUSD, log = F)
# 1.Index Returns
equityIdxRet1 <- ROC(all.equity.index, n = 1, type = "discrete")
equityIdxRet7 <- ROC(all.equity.index, n = 7, type = "discrete")
equityIdxRet14 <- ROC(all.equity.index, n = 14, type = "discrete")
equityIdxRet28 <- ROC(all.equity.index, n = 28, type = "discrete")

# 1.Index MACD
equityIdxMACD <- all.equity.index
for(idx in 1:ncol(all.equity.index)){
  macd_data = MACD(all.equity.index[, idx], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
  equityIdxMACD[,idx] <- macd_data$macd - macd_data$signal
}
tail(equityIdxMACD)

# 2.Bonds Returns
bondsRet1 <- ROC(all.bonds, n = 1, type = "discrete")
bondsRet7 <- ROC(all.bonds, n = 7, type = "discrete")
bondsRet14 <- ROC(all.bonds, n = 14, type = "discrete")
bondsRet28 <- ROC(all.bonds, n = 28, type = "discrete")

# 2.Bonds MACD
bondsMACD <- all.bonds
for(idx in 1:ncol(all.bonds)){
  macd_data = MACD(all.bonds[, idx], nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
  bondsMACD[,idx] <- macd_data$macd - macd_data$signal
}
tail(bondsMACD)

# 3.CFTC Diff
CFTCdiff1 <- ROC(all.CFTC, n = 1, type = "discrete")
CFTCdiff7 <- ROC(all.CFTC, n = 2, type = "discrete")
CFTCdiff14 <- ROC(all.CFTC, n = 3, type = "discrete")
CFTCdiff28 <- ROC(all.CFTC, n = 4, type = "discrete")

# 3.CFTC MACD
CFTCMACD <- all.CFTC
for(idx in 1:ncol(all.CFTC)){
  macd_data = MACD(all.CFTC[, idx], nFast = 3, nSlow = 6, nSig = 2, maType = "EMA", percent = TRUE)
  CFTCMACD[,idx] <- macd_data$macd - macd_data$signal
}
tail(CFTCMACD)

# 4.FX Returns
AUDUSDRet1 <- ROC(AUDUSD, n = 1, type = "discrete")
AUDUSDRet7 <- ROC(AUDUSD, n = 7, type = "discrete")
AUDUSDRet14 <- ROC(AUDUSD, n = 14, type = "discrete")
AUDUSDRet28 <- ROC(AUDUSD, n = 28, type = "discrete")
CADUSDRet1 <- ROC(CADUSD, n = 1, type = "discrete")
CADUSDRet7 <- ROC(CADUSD, n = 7, type = "discrete")
CADUSDRet14 <- ROC(CADUSD, n = 14, type = "discrete")
CADUSDRet28 <- ROC(CADUSD, n = 28, type = "discrete")

# 4.FX MACD
macd_data = MACD(AUDUSD, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
AUDUSDMACD <- macd_data$macd - macd_data$signal
macd_data = MACD(CADUSD, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
CADUSDMACD <- macd_data$macd - macd_data$signal

# 5.Price ratio
macd_data = MACD(price.ratio, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = TRUE)
prMACD <- macd_data$macd - macd_data$signal
tail(prMACD)


# Merge
tscale <- "2014-01-01/2016-07-22"
datasets <- merge(price.ratio[tscale], CFTCdiff1[tscale], all.x = T)
datasets$all.x <- NULL
datasets <- merge(datasets, CFTCdiff7[tscale], all.x = T)
datasets$all.x <- NULL
datasets <- merge(datasets, CFTCdiff14[tscale], all.x = T)
datasets$all.x <- NULL
datasets <- merge(datasets, CFTCdiff28[tscale], all.x = T)
datasets$all.x <- NULL
datasets <- merge(datasets, CFTCMACD[tscale], all.x = T)
datasets$all.x <- NULL


datasets <- merge(datasets, equityIdxRet1, equityIdxRet7, equityIdxRet14, equityIdxRet28, equityIdxMACD)
datasets <- merge(datasets, bondsRet1, bondsRet7, bondsRet14, bondsRet28, bondsMACD)
datasets <- merge(datasets, AUDUSDRet1, AUDUSDRet7, AUDUSDRet14, AUDUSDRet28, 
                  CADUSDRet1, CADUSDRet7, CADUSDRet14, CADUSDRet28,
                  AUDUSDMACD, CADUSDMACD)
datasets <- merge(datasets, macd_data)

for(i in 2:11){
  datasets[,i] <- fillMissingData(datasets[,i])
}
datasets <- na.omit(datasets)
head(datasets)
dim(datasets)


price = ROC(datasets$price.ratio, n = 1, type = "discrete")
class = ifelse(price > 0, 1, 0)
datasets <- data.frame(datasets)
datasets$price.ratio <- c(as.vector(class[-1]), NA)
datasets <- na.omit(datasets)

for(i in 2:ncol(datasets)){
  datasets[, i] <- as.numeric(scale(datasets[, i]))
}

# Train and test
breakpoint = nrow(datasets)*(2/3)
training_data = datasets[1:breakpoint,]
test_data = datasets[(breakpoint+1):nrow(datasets),]


# Use the SVM to find patterns and then make predictions ------------------
# SVM = svm(as.factor(price.ratio)~., data=training_data, kernel="radial",cost=1,gamma=0.5)
# predictions = predict(SVM, training_data, type = "class")
# trainingdata = data.frame(training_data, predictions)
# accuracy = (sum(trainingdata$predictions == trainingdata$price.ratio)/nrow(trainingdata))*100
# print(accuracy)
# 
# 
# predictions2 = predict(SVM, test_data, type = "class")
# testdata = data.frame(test_data, predictions2)
# accuracy2 = (sum(testdata$predictions == testdata$price.ratio)/nrow(testdata))*100
# print(accuracy2)

# fit <- glm(price.ratio~., family = "binomial", data = training_data)
# pred <- predict(fit, test_data, "response")
# library(pROC)
# r <- auc(test_data$price.ratio, pred)
# plot.roc(test_data$price.ratio, pred)



library(xgboost)
# binary
dtrain <- xgb.DMatrix(data.matrix(training_data[,-1]), label = training_data[,1], missing = NaN)
dtest <- xgb.DMatrix(data.matrix(test_data[,-1]), label = test_data[,1], missing = NaN)
watchlist = list(eval = dtest, train = dtrain)
param <- list(max.depth = 5,
              eta = 0.01,
              objective="binary:logistic",
              eval_metric="auc",
              colsample_bytree = 0.3,
              subsample = 0.99,
              num_parallel_tree = 100)
itr = 5
for(j in 1:itr){
  bst <- xgb.train(param, dtrain, nround = 100, watchlist, early.stop.round = 10, print.every.n = 5)
}
pred <- predict(bst, dtest)
auc(test_data$price.ratio, pred)
plot.roc(test_data$price.ratio, pred)

# reg
dtrain <- xgb.DMatrix(data.matrix(training_data[,-1]), label = as.vector(price[-1])[1:breakpoint], missing = NaN)
dtest <- xgb.DMatrix(data.matrix(test_data[,-1]), label = as.vector(price[-1])[(breakpoint+1):nrow(datasets)], missing = NaN)
watchlist = list(eval = dtest, train = dtrain)
param <- list(max.depth = 5,
              eta = 0.03,
              objective="reg:linear",
              eval_metric="rmse",
              colsample_bytree = 0.3,
              subsample = 0.99,
              num_parallel_tree = 20)
itr = 5
for(j in 1:itr){
  bst <- xgb.train(param, dtrain, nround = 100, watchlist, early.stop.round = 10, print.every.n = 5)
}
pred <- predict(bst, dtest)
plot(as.vector(price[-1])[(breakpoint+1):nrow(datasets)], pred)
as.vector(price[-1])[(breakpoint+1):nrow(datasets)]
pred
