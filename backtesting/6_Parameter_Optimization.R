source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
?apply.paramset()
.fastSMA <- (1:30)
.slowSMA <- (20:80)
.nsamples <- 5

portfolio.st <- "Port.Luxor.MA.Opt"
account.st <- "Acct.Luxor.MA.Opt"
strategy.st <- "Strat.Luxor.MA.Opt"

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


# Add Distribution --------------------------------------------------------
add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nFast",
                 variable = list(n = .fastSMA),
                 label = "nFAST")

add.distribution(strategy.st,
                 paramset.label = "SMA",
                 component.type = "indicator",
                 component.label = "nSlow",
                 variable = list(n = .slowSMA),
                 label = "nSLOW")


# Add Distribution Constraint ---------------------------------------------
# nFAST is always smaller than nSLOW
add.distribution.constraint(strategy.st,
                            paramset.label = "SMA",
                            distribution.label.1 = "nFAST",
                            distribution.label.2 = "nSLOW",
                            operator = "<",
                            label = "SMA.Constraint")

# Running Parallel --------------------------------------------------------
library(parallel)
if( Sys.info()['sysname'] == "Windows") {
    library(doParallel)
    registerDoParallel(cores=detectCores())
} else {
    library(doMC)
    registerDoMC(cores=detectCores())
}



# Apply Paramset ----------------------------------------------------------
cwd <- getwd()
setwd("./_data/")
results_file <- paste("results", strategy.st, "RData", sep = ".")
if( file.exists(results_file) ) {
    load(results_file)
} else {
    results <- apply.paramset(strategy.st,
                              paramset.label = "SMA",
                              portfolio.st = portfolio.st,
                              account.st = account.st, 
                              nsamples = .nsamples)
    if(checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)) {
        save(list = "results", file = results_file)
        save.strategy(strategy.st)
    }
}
setwd(cwd)

