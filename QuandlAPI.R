library(AutoPairTrading)
QuandlConnect()

# mydata = Quandl("FRED/GDP", type="xts")

# mydata = Quandl("FRED/GDP", start_date="2001-12-31", end_date="2005-12-31")
# mydata = Quandl(c("FRED/GDP.1", "WIKI/AAPL.4"))

# mydata = Quandl("FRED/GDP", collapse="annual")
# mydata = Quandl("FRED/GDP", transform="rdiff")

# mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL")
# mydata = Quandl.datatable("ZACKS/FC", ticker="AAPL", paginate=T)
# mydata = Quandl.datatable("ZACKS/FC", ticker=c("AAPL, MSFT"), 
#                           per_end_date.gt="2015-01-01",
#                           qopts.columns=c("m_ticker", "per_end_date", "tot_revnu"))


# 1. CFTC -----------------------------------------------------------------
calNet <- function(x,y){
  net <- (x-y)/(x+y)
  return(net)
}

CFTC.AUD <- Quandl("CFTC/TIFF_CME_AD_ALL")
CFTC.AUD <- as.xts(calNet(CFTC.AUD$`Total Reportable Long Positions`,CFTC.AUD$`Total Reportable Short Positions`),CFTC.AUD$Date)
CFTC.CAD <- Quandl("CFTC/TIFF_CME_CD_ALL")
CFTC.CAD <- as.xts(calNet(CFTC.CAD$`Total Reportable Long Positions`,CFTC.CAD$`Total Reportable Short Positions`),CFTC.CAD$Date)

all.CFTC <- na.omit(merge(CFTC.AUD, CFTC.CAD))


# 2. Equity Index ---------------------------------------------------------
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


save(AUDUSD, CADUSD, all.CFTC, all.equity.index, all.bonds, file = "data/modelingRawData.RData")
