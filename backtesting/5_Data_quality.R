source('~/analytics/Workspace/backtesting/2_Using_Quantstrat.R')
getSymbols("SPY", 
           src = "yahoo", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)
yahoo.SPY <- SPY
rm(SPY)
getSymbols("SPY", 
           src = "google", 
           index.class = c("POSIXt", "POSIXct"),
           from = "2010-01-01", 
           to = "2011-01-01", 
           adjust = TRUE)
google.SPY <- SPY
bind_rows(as.data.frame(yahoo.SPY) %>% 
              mutate(Src = "Yahoo"), 
          as.data.frame(google.SPY) %>% 
              mutate(Src = "Google")) %>% 
    gather(key, value, 1:4, na.rm = TRUE) %>% 
    ggplot(aes(x = key, y = value, fill = Src)) + 
    geom_boxplot() + 
    theme_bw() + 
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    ggtitle("Google vs. Yahoo! (non-NA)")



as.data.frame(google.SPY) %>% 
    mutate(Date = index(google.SPY)) %>% 
    select(Date, starts_with("SPY"), -SPY.Volume) %>% 
    filter(is.na(SPY.Open))



# Examining Traders -------------------------------------------------------

rm.strat(portfolio.st)
rm.strat(account.st)
symbols <- basic_symbols()
getSymbols(Symbols = symbols, src = "yahoo", index.class = "POSIXct", 
           from = start_date, to = end_date, adjust = adjustment)
initPortf(name = portfolio.st, symbols = symbols, initDate = init_date)
initAcct(name = account.st, portfolios = portfolio.st, initDate = init_date, 
         initEq = init_equity)
initOrders(portfolio = portfolio.st, symbols = symbols, initDate = init_date)
applyStrategy(strategy.st, portfolios = portfolio.st)
checkBlotterUpdate(portfolio.st, account.st, verbose = TRUE)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

chart.Posn(portfolio.st, Symbol = "SPY", Dates="2008-01-01::2008-07-01", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")



le <- as.data.frame(mktdata["2008-02-25::2008-03-07", c(1:4, 7:10)])
DT::datatable(le, 
              rownames = TRUE,
              extensions = c("Scroller", "FixedColumns"), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE, 
                             deferRender = TRUE, 
                             scrollX = 200, 
                             scroller = TRUE,
                             fixedColumns = TRUE), 
              caption = htmltools::tags$caption(
                  "Table 6.1: mktdata object for Feb. 25, 2008 to Mar. 7, 2008"))


ob <- as.data.table(getOrderBook(portfolio.st)$Quantstrat$SPY)
DT::datatable(ob, 
              rownames = FALSE,
              filter = "top",
              extensions = c("Scroller", "FixedColumns"), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE, 
                             deferRender = TRUE, 
                             scrollX = 200, 
                             scroller = TRUE, 
                             fixedColumns = TRUE), 
              caption = htmltools::tags$caption(
                  "Table 6.2: Order book for SPY"))


chart.Posn(portfolio.st, Symbol = "SPY", Dates="2009-08-01::2009-12-31", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")

chart.Posn(portfolio.st, Symbol = "SPY", 
           TA="add_SMA(n = 10, col = 2); add_SMA(n = 30, col = 4)")