set.seed(2)
a0 <- 0.2
a1 <- 0.5
b1 <- 0.3
w <- rnorm(10000)
eps <- rep(0, 10000)
sigsq <- rep(0, 10000)
for (i in 2:10000) {
  sigsq[i] <- a0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1]
  eps[i] <- w[i]*sqrt(sigsq[i])
}

acf(eps)
acf(eps^2)


require(tseries)
eps.garch <- garch(eps, trace=FALSE)
confint(eps.garch)



# 1. get index ------------------------------------------------------------
require(quantmod)
getSymbols("^GSPC")
ftrt = diff(log(Cl(GSPC)))
plot(ftrt)

ft <- as.numeric(ftrt)
ft <- ft[!is.na(ft)]


# 2. run ARIMA ------------------------------------------------------------
ftfinal.aic <- Inf
ftfinal.order <- c(0,0,0)
for (p in 1:4) for (d in 0:1) for (q in 1:4) {
  ftcurrent.aic <- AIC(arima(ft, order=c(p, d, q)))
  if (ftcurrent.aic < ftfinal.aic) {
    ftfinal.aic <- ftcurrent.aic
    ftfinal.order <- c(p, d, q)
    ftfinal.arima <- arima(ft, order=ftfinal.order)
    }
}
ftfinal.order
acf(resid(ftfinal.arima))


# 3. test conditional heteroskedastic behaviour ---------------------------
acf(resid(ftfinal.arima)^2) # proven serial correlation in the squared residuals, leading to the conditional heteroskedastic behaviour



# 4. fit garch model ------------------------------------------------------
ft.garch <- garch(ft, trace=F)
ft.res <- ft.garch$res[-1]
acf(ft.res)
acf(ft.res^2)
