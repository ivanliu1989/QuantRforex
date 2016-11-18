library(quantmod)
AAPL <- getSymbols("AAPL", auto.assign = F)
head(AAPL)

chartSeries(AAPL,subset = "2015::2016", TA = c('addBBands(); addDEMA()'))
addVo()
addDPO()

my_indicator <- function(x){
  return(x + 90)
}
add_my_indicator <- newTA(FUN = my_indicator, preFUN = Cl, legend.name = "My Indicator", on = 1)
add_my_indicator()
