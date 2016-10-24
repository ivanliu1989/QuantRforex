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
