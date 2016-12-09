# source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
.StopLoss = seq(0.05, 0.6, length.out = 48)/100
# alpha = seq(0.05, 1, length.out = 20)
rm(list = ls(.blotter), envir = .blotter); ls(.blotter)
rm(list = ls(.strategy), envir = .strategy); ls(.strategy)

portfolio.st <- "Port.MACDStrat"
account.st <- "Acct.MACDStrat"
strategy.st <- "Strat.MACDStrat"
rm.strat(portfolio.st)
rm.strat(account.st)

Sys.setenv(TZ="UTC")
symb1 <- 'AUD_USD'
symb2 <- 'USD_CAD'
# AUD USD
AUD_USD = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = symb1, price = 'M', granularity = 'D', count = 1500)
AUD_USD$complete <- NULL
names(AUD_USD) <- c("time", "Volume", "Close", "High", "Low", "Open")
AUD_USD[, 2:6] <- sapply(AUD_USD[, 2:6], as.numeric)
AUD_USD = xts(AUD_USD[,-1],as.POSIXct(as.Date(AUD_USD$time)))
# USD CAD
USD_CAD = getOandaInstrumentCandles(.oandaEnv$ACCOUNT_TYPE, .oandaEnv$ACCESS_TOKEN, INSTRUMENTS = symb2, price = 'M', granularity = 'D', count = 1500)
USD_CAD$complete <- NULL
names(USD_CAD) <- c("time", "Volume", "Close", "High", "Low", "Open")
USD_CAD[, 2:6] <- sapply(USD_CAD[, 2:6], as.numeric)
USD_CAD = xts(USD_CAD[,-1],as.POSIXct(as.Date(USD_CAD$time)))


spreads <- OHLC(AUD_USD)-OHLC(1/USD_CAD)

symbols <- c("spreads")
currency("USD")
# instrument(symbols, currency = "USD", multiplier = 1, tick_size = 0.00001)
stock(symbols,currency = "USD",multiplier = 1, tick_size = 0.00001)
# fetch market data and plot the spread
start_date = as.character(min(index(spreads)))
end_date = as.character(max(index(spreads)))
init_date = as.character(min(as.Date(index(spreads)))-1)
init_equity = 1000000
.fast <- 10
.slow <- 30
.threshold <- 0.0005
.orderqty <- init_equity * 0.02
.txnfees <- -init_equity * 0.00002
.stoploss <- 3e-3 # 0.003 or 0.3%
.nsamples <- 10


initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initEq=init_equity,
         initDate = init_date)
initOrders(portfolio = portfolio.st,
           initDate = init_date)

strategy(strategy.st, store = TRUE)


# Add Indicators ----------------------------------------------------------
add.indicator(strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .fast),
              label = "nFast")
add.indicator(strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = .slow),
              label = "nSlow")


# Add Signals -------------------------------------------------------------
add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long"
)
add.signal(strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")


# Add Rules ---------------------------------------------------------------
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long" ,
                          ordertype = "stoplimit",
                          prefer = "High",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = +.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocolong"),
         type = "enter",
         label = "EnterLONG")
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          prefer = "Low",
                          threshold = .threshold,
                          TxnFees = .txnfees,
                          orderqty = -.orderqty,
                          osFUN = osMaxPos,
                          orderset = "ocoshort"),
         type = "enter",
         label = "EnterSHORT")
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "long" ,
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "exit",
         label = "Exit2SHORT")
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          replace = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "exit",
         label = "Exit2LONG")
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long" ,
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "long",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocolong"),
         type = "chain",
         parent = "EnterLONG",
         label = "StopLossLONG",
         enabled = FALSE)
add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          replace = FALSE,
                          orderside = "short",
                          ordertype = "stoplimit",
                          tmult = TRUE,
                          threshold = quote(.stoploss),
                          TxnFees = .txnfees,
                          orderqty = "all",
                          orderset = "ocoshort"),
         type = "chain",
         parent = "EnterSHORT",
         label = "StopLossSHORT",
         enabled = FALSE)


# Add Position Limit ------------------------------------------------------
addPosLimit(portfolio = portfolio.st,
            symbol = symbols,
            timestamp = init_date,
            maxpos = .orderqty)


# Add Distribution --------------------------------------------------------
add.distribution(strategy.st,
                 paramset.label = "StopLoss",
                 component.type = "chain",
                 component.label = "StopLossLONG",
                 variable = list(threshold = .StopLoss),
                 label = "StopLossLONG")
add.distribution(strategy.st,
                 paramset.label = "StopLoss",
                 component.type = "chain",
                 component.label = "StopLossSHORT",
                 variable = list(threshold = .StopLoss),
                 label = "StopLossSHORT")


# Add Distribution Constraint ---------------------------------------------
add.distribution.constraint(strategy.st,
                            paramset.label = "StopLoss",
                            distribution.label.1 = "StopLossLONG",
                            distribution.label.2 = "StopLossSHORT",
                            operator = "==",
                            label = "StopLoss")


# Enable Rules ------------------------------------------------------------
enable.rule(strategy.st, 'chain', 'StopLoss')



# Apply Paramset ----------------------------------------------------------

results <- apply.paramset(strategy.st,
                          paramset.label = "StopLoss",
                          portfolio.st = portfolio.st,
                          account.st = account.st,
                          nsamples = .nsamples,
                          verbose = TRUE)

if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
    save.strategy(strategy.st)
}


# chart.Posn(portfolio.st, Symbol = "SPY",
#            TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")
