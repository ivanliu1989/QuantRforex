rm(list = ls()); gc()
library(AutoPairTrading)
# source("QuandlIR.R")
# source("QuandlAPI.R")
ls()
load("data/modelingRawData.RData")
tscale <- "2014-01-01/2016-07-22"
all.bonds <- all.bonds[tscale]
all.equity.index <- all.equity.index[tscale]
all.CFTC <- all.CFTC[tscale]
AUDUSD <- AUDUSD[tscale]
CADUSD <- CADUSD[tscale]
price.ratio <- AutoPairTrading::getPriceRatio(AUDUSD, CADUSD, log = F)


datasets <- merge(price.ratio, all.CFTC, all.x = T)
datasets$all.x <- NULL
for(i in 1:ncol(datasets)){
  datasets[,i] <- fillMissingData(datasets[,i])  
}
datasets <- na.omit(datasets)

# 3. Momentum Indicators -------------------------------------------------
momRSI = data.frame()
for(c in 1:dim(datasets)[2]){
  a = momentum.RSI(datasets[,c])
  momRSI = cbind(momRSI, a)
}
colnames(momRSI) <- paste0(colnames(datasets), ".RSI")


momMACD = data.frame()
for(c in 1:dim(datasets)[2]){
  a = momentum.MACD(datasets[,c])$longshort
  momMACD = cbind(momMACD, a)
}
colnames(momMACD) <- paste0(colnames(datasets), ".MACD")

momCrossover = data.frame()
for(c in 1:dim(datasets)[2]){
  a = momentum.Crossover(datasets[,c])
  a = a$hamming.Dist * a$spearman * a$thickness
  momCrossover = cbind(momCrossover, a)
}
colnames(momCrossover) <- paste0(colnames(datasets), ".xOver")


# 4. Model Data Generation -----------------------------------------------
all <- na.omit(merge(datasets, momRSI, momMACD, momCrossover))
feats <- names(all)
forwardPred <- c(1:4)
objFeatures <- matrix(NA, nrow = nrow(all), ncol = length(forwardPred))
for(i in 1:length(forwardPred)){
  objFeatures[,i] = lag(all$price.ratio, forwardPred[i]) - all$price.ratio
}
all <- na.omit(cbind(as.data.frame(all), objFeatures))
for(i in 1:ncol(all)){
  all[,i] <- zscores(all[,i])
}

# pred <- na.omit(merge(datasets, momRSI, momMACD, momCrossover))
# pred <- na.omit(pred)
# for(i in 1:ncol(pred)){
#   pred[,i] <- zscores(pred[,i])
# }
# pred = pred[nrow(pred),]

idx <- 1: (nrow(all)*0.8)
features <- all[, feats]
labels <- all[, !names(all) %in% feats]
# labels[labels > 0] <- 1
# labels[labels <=0] <- 0

# 5. Modeling -------------------------------------------------------------
library(xgboost)
# par(mfcol = c(3,2))
# dpred <- xgb.DMatrix(pred, missing = NaN)
itr = 5
# predictions <- matrix(NA, nrow = itr, ncol = ncol(labels))
predictions <- matrix(NA, nrow = itr*ncol(labels), ncol = 2)
for(i in 1:ncol(labels)){
  # cat(paste0("\nLabel: ", i))
  train <- as.data.frame(cbind(as.matrix(features[idx,]), labels[idx, i]))
  test <- as.data.frame(cbind(as.matrix(features[-idx,]), labels[-idx, i]))
  colnames(train) <- c(names(features), "label")
  # mylogit <- glm(label ~ ., data = train,  family = "binomial")
  # p <- predict(mylogit, test, "response")
  # library(AUC)
  mylm <- lm(label ~ ., data = train)
  plot(AUC::roc(p, factor(labels[-idx, i])))
  AUC::accuracy(p, factor(labels[idx, i]))
  
  dtrain <- xgb.DMatrix(data.matrix(features[idx,]), label = labels[idx, i], missing = NaN)
  dtest <- xgb.DMatrix(data.matrix(features[-idx,]), label = labels[-idx, i], missing = NaN)
  watchlist = list(eval = dtest, train = dtrain)
  param <- list(max.depth = 5,
                eta = 0.01,
                objective="binary:logistic",
                eval_metric="auc",
                subsample = 0.99)
  for(j in 1:itr){
    # cat(paste0("\nLabel: ", i, " Iteration: ", j))
    bst <- xgb.train(param, dtrain, nround = 100, watchlist, early.stop.round = 20, print.every.n = 1)
    # predictions[itr*(i-1)+j,] = c(i, predict(bst, dpred))
  }
}
colnames(predictions) <- c("x", "y")
# plot(pred, labels[-idx, i])
# xgb.plot.importance(xgb.importance(names(features), model = bst)[1:20])
return(as.data.frame(predictions))