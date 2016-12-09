source('~/analytics/QuantRforex/backtesting/2_Using_Quantstrat.R')
# If SMA(10) >= SMA(30):
#     BTC short, BTO long
# Else if SMA(10) < SMA(30):
#     STC long, STO short

symbols <- basic_symbols()
getSymbols(Symbols = symbols,
           src = "yahoo",
           index.class = "POSIXct",
           from = start_date,
           to = end_date,
           adjust = adjustment)


# 1. define the meta-data for our symbols ---------------------------------
stock(symbols,
      currency = "USD",
      multiplier = 1)
# exchange_rate("EURUSD") # define an exchange rate



# 2. assign proper names for our portfolio, account and strategy o --------
portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"
# clearing out the portfolio and account values
rm.strat(portfolio.st)
rm.strat(account.st)


# 3. Initialize portfolio, account and orders -----------------------------
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
# store strategy to save for later
strategy(strategy.st, store = TRUE)


# 3. Add Indicators -------------------------------------------------------
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), # e.g. mktdata = IWM
                               n = 10),
              label = "nFast")
# mktdata is a special dataset created for each symbol that will store all of our indicators and signals.
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)),
                               n = 30),
              label = "nSlow")
# strategy: As we stored our strategy name in the strategy.st variable all we need to do is pass that variable.
#           Otherwise we would provide a string. Use variables when this will become redundant as we move along.
# name: Indicator function; for this example SMA. We only pass the name of the function as a character string.
#       Parameters for the function are passed into the arguments parameter…
# arguments: If we look at ?SMA we see required parameters are x and n with the default n being 10.
#            x is the price object. In our example we are using closing prices.
# label: Label of the variable that will be added to our dataset. This must be unique for each indicator we add.



# 4. Add Signals ----------------------------------------------------------
add.signal(strategy = strategy.st,
           name="sigCrossover", # ?sigCrossover
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "gte"),
           label = "long")
add.signal(strategy = strategy.st,
           name="sigCrossover",
           arguments = list(columns = c("nFast", "nSlow"),
                            relationship = "lt"),
           label = "short")
# sigComparison: boolean, compare two variables by relationship
# gt greater than
# lt less than
# eq equal to
# gte greater than or equal to
# lte less than or equal to

# sigCrossover: boolean, TRUE when one signal crosses another. Uses the same relationships as sigComparison
#
# sigFormula: apply a formula to multiple variables.
#
# sigPeak: identify local minima or maxima of an indicator
#
# sigThreshold: boolean, when an indicator crosses a value. Uses relationships as identified above.
#
# sigTimestamp: generates a signal based on a timestamp.

### A function to compare inputs and returns TRUE/FALSE


# 5. Add Rules ------------------------------------------------------------
# add.rules will determine the positions we take depending on our signals, what type of order we’ll place and how many shares we will buy.
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderqty = 100,
                          ordertype = "stoplimit",
                          orderside = "long",
                          threshold = 0.0005,
                          prefer = "High",
                          TxnFees = -10,
                          replace = FALSE),
         type = "enter",
         label = "EnterLONG")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderqty = -100,
                          ordertype = "stoplimit",
                          threshold = -0.005,
                          orderside = "short",
                          replace = FALSE,
                          TxnFees = -10,
                          prefer = "Low"),
         type = "enter",
         label = "EnterSHORT")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "short",
                          sigval = TRUE,
                          orderside = "long",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = -10,
                          replace = TRUE),
         type = "exit",
         label = "Exit2SHORT")

add.rule(strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "long",
                          sigval = TRUE,
                          orderside = "short",
                          ordertype = "market",
                          orderqty = "all",
                          TxnFees = -10,
                          replace = TRUE),
         type = "exit",
         label = "Exit2LONG")


# 6. Apply Strategy -------------------------------------------------------
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
    load(results_file)
} else {
    results <- applyStrategy(strategy.st, portfolios = portfolio.st)
    updatePortf(portfolio.st)
    updateAcct(account.st)
    updateEndEq(account.st)
    if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
        save(list = "results", file = results_file)
        save.strategy(strategy.st)
    }
}
setwd(cwd)

# Next we update our portfolio and account objects.
# We do this with the updatePortf(), updateAcct() and updateEndEq() functions.
# Next we update our portfolio and account objects.
# We do this with the updatePortf(), updateAcct() and updateEndEq() functions.

