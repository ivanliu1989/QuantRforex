portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
cwd <- getwd()
setwd("./_data/")
load.strategy(strategy.st)
setwd(cwd)
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
           initDate = init_date)

applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)

updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

symbol = forex.str

# Chart Positions ---------------------------------------------------------
for(symbol in symbols) {
    chart.Posn(portfolio.st, Symbol = symbol, 
               TA = "add_SMA(n = 10, col = 4); add_SMA(n = 30, col = 2)")
}


# Trade Statistics --------------------------------------------------------
tstats <- tradeStats(portfolio.st)
kable(t(tstats))

# Trade Related
tab.trades <- tstats %>% 
    mutate(Trades = Num.Trades, 
           Win.Percent = Percent.Positive, 
           Loss.Percent = Percent.Negative, 
           WL.Ratio = Percent.Positive/Percent.Negative) %>% 
    select(Trades, Win.Percent, Loss.Percent, WL.Ratio)

kable(t(tab.trades))

# Profit Related
tab.profit <- tstats %>% 
    select(Net.Trading.PL, Gross.Profits, Gross.Losses, Profit.Factor)
kable(t(tab.profit))

# Averages
tab.wins <- tstats %>% 
    select(Avg.Trade.PL, Avg.Win.Trade, Avg.Losing.Trade, Avg.WinLoss.Ratio)
kable(t(tab.wins))

# Performance Summary
rets <- PortfReturns(Account = account.st)
rownames(rets) <- NULL
charts.PerformanceSummary(rets, colorset = bluefocus)

# Per Trade Statistics
for(symbol in symbols) {
    pts <- perTradeStats(portfolio.st, Symbol = symbol)
    kable(pts, booktabs = TRUE, caption = symbol)
}
kable(pts)

# Performance Statistics
tab.perf <- table.Arbitrary(rets,
                            metrics=c(
                                "Return.cumulative",
                                "Return.annualized",
                                "SharpeRatio.annualized",
                                "CalmarRatio"),
                            metricsNames=c(
                                "Cumulative Return",
                                "Annualized Return",
                                "Annualized Sharpe Ratio",
                                "Calmar Ratio"))
kable(tab.perf)

# Risk Statistics
tab.risk <- table.Arbitrary(rets,
                            metrics=c(
                                "StdDev.annualized",
                                "maxDrawdown",
                                "VaR",
                                "ES"),
                            metricsNames=c(
                                "Annualized StdDev",
                                "Max DrawDown",
                                "Value-at-Risk",
                                "Conditional VaR"))
kable(tab.risk)

# Buy and Hold Performance
rm.strat("buyHold")
# initialize portfolio and account
initPortf("buyHold", "SPY", initDate = init_date)
initAcct("buyHold", portfolios = "buyHold",
         initDate = init_date, initEq = init_equity)
# place an entry order
CurrentDate <- time(getTxns(Portfolio = portfolio.st, Symbol = "SPY"))[2]
equity = getEndEq("buyHold", CurrentDate)
ClosePrice <- as.numeric(Cl(SPY[CurrentDate,]))
UnitSize = as.numeric(trunc(equity/ClosePrice))
addTxn("buyHold", Symbol = "SPY", TxnDate = CurrentDate, TxnPrice = ClosePrice,
       TxnQty = UnitSize, TxnFees = 0)
# place an exit order
LastDate <- last(time(SPY))
LastPrice <- as.numeric(Cl(SPY[LastDate,]))
addTxn("buyHold", Symbol = "SPY", TxnDate = LastDate, TxnPrice = LastPrice,
       TxnQty = -UnitSize , TxnFees = 0)
# update portfolio and account
updatePortf(Portfolio = "buyHold")
updateAcct(name = "buyHold")
updateEndEq(Account = "buyHold")
chart.Posn("buyHold", Symbol = "SPY")


# Strategy vs. Market
rets <- PortfReturns(Account = account.st)
rets.bh <- PortfReturns(Account = "buyHold")
returns <- cbind(rets, rets.bh)
charts.PerformanceSummary(returns, geometric = FALSE, wealth.index = TRUE, 
                          main = "Strategy vs. Market")


# Risk/Return Scatterplot
chart.RiskReturnScatter(returns, Rf = 0, add.sharpe = c(1, 2), 
                        main = "Return vs. Risk", colorset = c("red", "blue"))


# Relative Performance
for(n in 1:(ncol(returns) - 1)) {
    chart.RelativePerformance(returns[, n], returns[, ncol(returns)], 
                              colorset = c("red", "blue"), lwd = 2, 
                              legend.loc = "topleft")
}

# Portfolio Summary
pf <- getPortfolio(portfolio.st)
xyplot(pf$summary, type = "h", col = 4)

# Order Book
ob <- getOrderBook(portfolio.st)

# Maximum Adverse Excursion
for(symbol in symbols) {
    chart.ME(Portfolio = portfolio.st, Symbol = symbol, type = "MAE", 
             scale = "percent")
}

# Maximum Favorable Excursion
for(symbol in symbols) {
    chart.ME(Portfolio = portfolio.st, Symbol = symbol, type = "MFE", 
             scale = "percent")
}


# Account Summary ---------------------------------------------------------
a <- getAccount(account.st)
xyplot(a$summary, type = "h", col = 4)

# Equity Curve
equity <- a$summary$End.Eq
plot(equity, main = "Equity Curve")

# Account Performance Summary
ret <- Return.calculate(equity, method = "log")
charts.PerformanceSummary(ret, colorset = bluefocus, 
                          main = "Strategy Performance")

# Cumulative Returns
rets <- PortfReturns(Account = account.st)
chart.CumReturns(rets, colorset = rich10equal, legend.loc = "topleft", 
                 main="SPDR Cumulative Returns")

# Distribution Analysis
chart.Boxplot(rets, main = "SPDR Returns", colorset= rich10equal)

# Annualized Returns
(ar.tab <- table.AnnualizedReturns(rets))

# Performance Scatter Plot
max.risk <- max(ar.tab["Annualized Std Dev",])
max.return <- max(ar.tab["Annualized Return",])
chart.RiskReturnScatter(rets,
                        main = "SPDR Performance", colorset = rich10equal,
                        xlim = c(0, max.risk * 1.1), ylim = c(0, max.return))

# Notional Costs
#quantstratII pp. 67/69
mnc <- pts$Max.Notional.Cost
pe <- sapply(pts$Start,getEndEq, Account = account.st)/3
barplot(rbind(pe,mnc),beside=T,col=c(2,4),names.arg=format(pts$Start,"%m/%d/%y"),
        ylim=c(0,1.5e5),ylab="$",xlab="Trade Date")
legend(x="topleft",legend=c("(Portfolio Equity)/9","Order Size"),
       pch=15,col=c(2,4),bty="n")
title("Percent of Portfolio Equity versus Trade Size for XLU")