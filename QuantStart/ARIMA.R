# AMAZON
getSymbols("AMZN", from="2013-01-01")
amzn = diff(log(Cl(AMZN)))
azfinal.aic <- Inf
azfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  azcurrent.aic <- AIC(arima(amzn, order=c(p, d, q)))
  if (azcurrent.aic < azfinal.aic) {
    azfinal.aic <- azcurrent.aic
    azfinal.order <- c(p, d, q)
    azfinal.arima <- arima(amzn, order=azfinal.order)
    }
}
azfinal.order
acf(resid(azfinal.arima), na.action=na.omit)
Box.test(resid(azfinal.arima), lag=20, type="Ljung-Box")

plot(forecast(azfinal.arima, h=25))



# S&P500
getSymbols("^GSPC", from="2013-01-01")
sp = diff(log(Cl(GSPC)))
spfinal.aic <- Inf
spfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  spcurrent.aic <- AIC(arima(sp, order=c(p, d, q)))
  if (spcurrent.aic < spfinal.aic) {
    spfinal.aic <- spcurrent.aic
    spfinal.order <- c(p, d, q)
    spfinal.arima <- arima(sp, order=spfinal.order)
    }
}
spfinal.order
acf(resid(spfinal.arima), na.action=na.omit)
Box.test(resid(spfinal.arima), lag=20, type="Ljung-Box")
plot(forecast(spfinal.arima, h=25))

spauto.arima <- auto.arima(sp, stationary = T) 
acf(resid(spauto.arima), na.action=na.omit)
Box.test(resid(spauto.arima), lag=20, type="Ljung-Box")
plot(forecast(spauto.arima, h=25))

