source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
trailingStopPercent <- 0.07
trade_size <- init_equity/length(symbols)

portfolio.st <- "Quantstrat"
account.st <- "Strategies"
strategy.st <- "MACD.TS"
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


# osFixedDollar -----------------------------------------------------------
osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...) {
    if(!exists("trade_size")) stop("You must set trade_size")
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- round(trade_size/ClosePrice,-2)
    return(orderqty)
}


# Add Indicators ----------------------------------------------------------
add.indicator(strategy = strategy.st,
              name = "MACD",
              arguments = list(x = quote(Cl(mktdata))),
              label = "osc")


# Add Signals -------------------------------------------------------------
add.signal(strategy = strategy.st,
           name="sigThreshold",
           arguments = list(column ="signal.osc", 
                            relationshipo = "gt", 
                            threshold = 0, 
                            cross = TRUE), 
           label = "signal.gt.zero")
add.signal(strategy = strategy.st,
           name="sigThreshold",
           arguments = list(column = "signal.osc", 
                            relationship = "lt", 
                            threshold = 0, 
                            cross = TRUE), 
           label = "signal.lt.zero")


# Add Rules ---------------------------------------------------------------
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "signal.gt.zero",
                          sigval = TRUE,
                          orderqty = 100,
                          orderside = "long",
                          ordertype = "market",
                          osFUN = "osFixedDollar", 
                          orderset = "ocolong"),
         type = "enter",
         label = "LE")
add.rule(strategy = strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "signal.lt.zero", 
                          sigval = TRUE, 
                          replace = TRUE, 
                          orderside = "long", 
                          ordertype = "market", 
                          orderqty = "all", 
                          orderset = "ocolong"), 
         type = "exit", 
         label = "LX")
add.rule(strategy = strategy.st, 
         name = "ruleSignal", 
         arguments = list(sigcol = "signal.gt.zero", 
                          sigval = TRUE, 
                          replace = FALSE, 
                          orderside = "long", 
                          ordertype = "stoptrailing", 
                          tmult = TRUE, 
                          threshold = quote(trailingStopPercent), 
                          orderqty = "all", 
                          orderset = "ocolong"), 
         type = "chain", 
         parent = "LE", 
         label = "StopTrailingLong", 
         enabled = FALSE)


# Enable Rules ------------------------------------------------------------
enable.rule(strategy.st, type = "chain", label = "StopTrailingLong")

cwd <- getwd()
setwd("./_data/")
save.strategy(strategy.st)
setwd(cwd)