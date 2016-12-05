source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
.StopLoss = seq(0.05, 0.6, length.out = 48)/100
strategy.st <- "Luxor.Stop.Loss.Opt"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
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
for(symbol in symbols){
    addPosLimit(portfolio = portfolio.st,
                symbol = symbol,
                timestamp = init_date,
                maxpos = .orderqty)
}


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

cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
    load(results_file)
} else {
    results <- apply.paramset(strategy.st, 
                              paramset.label = "StopLoss", 
                              portfolio.st = portfolio.st, 
                              account.st = account.st, 
                              nsamples = .nsamples, 
                              verbose = TRUE)
    
    if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
        save(list = "results", file = results_file)
        save.strategy(strategy.st)
    }
}


# chart.Posn(portfolio.st, Symbol = "SPY", 
#            TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")
