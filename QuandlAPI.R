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
CFTC.AUD <- Quandl("CFTC/TIFF_CME_AD_ALL"
                   # , transform = "normalize"
                   # , type = "xts"
                   )
CFTC.CAD <- Quandl("CFTC/TIFF_CME_CD_ALL"
                   # , transform = "normalize"
                   # , type = "xts"
)



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
