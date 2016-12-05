source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
.fast <- 10
.slow <- 30
.threshold <- 0.0005
.orderqty <- 100
.txnfees <- -10
.stoploss <- 3e-3 # 0.003 or 0.3%

portfolio.st <- "Port.Luxor.Stop.Loss"
account.st <- "Acct.Luxor.Stop.Loss"
strategy.st <- "Strat.Luxor.Stop.Loss"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,
          symbols = symbols,
          initDate = init_date)
initAcct(name = account.st,
         portfolios = portfolio.st,
         initDate = init_date,
         initEq = init_equity)
initOrders(portfolio = portfolio.st,
           symbols = symbols,
           initDate = init_date)

strategy(strategy.st, store = TRUE)

# Add Indicators
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

# Add Signals
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

# Add Rules
# osFUN  function or text descriptor of function to use for order sizing.
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
                          osFUN = osMaxPos, # ?osMaxPos
                          # osFUN  function or text descriptor of function to use for order sizing.
                          orderset = "ocolong"),# This will help group our long and short orders together.
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

# stop loss rules
# First, we’ve created rule StopLossLONG as a child rule of the parent rule EnterLONG, part of the orderset ocolong. 
# Currently it is not enabled.
# The critical portion of StopLossLONG is the tmult and threshold parameter. 
# When a long order is filled threshold and tmult work together to determine the stoplimit price (ordertype). 
# .stoploss is multiplied (tmult) against the price of the filled long order. That price serves as the stop-loss price.
# 
# For example,
# StopLossLONG=fill price −(.stoploss ∗fill price)
# StopLossLONG=134.39−(0.003∗134.39)
# StopLossLONG=$133.9868
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



# Enable Rules ------------------------------------------------------------
enable.rule(strategy.st, 
            type = "chain", 
            label = "StopLoss")



cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
    load(results_file)
} else {
    results <- applyStrategy(strategy.st, portfolios = portfolio.st)
    if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
        save(list = "results", file = results_file)
        save.strategy(strategy.st)
    }
}
setwd(cwd)

chart.Posn(portfolio.st, Symbol = "SPY", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")