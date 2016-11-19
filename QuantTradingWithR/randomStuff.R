library(quantmod)
AAPL <- getSymbols("AAPL", auto.assign = F)
head(AAPL)

chartSeries(AAPL,subset = "2015::2016", TA = c('addBBands(); addDEMA()'), theme = "white")
addVo()
addDPO()

my_indicator <- function(x){
  return(x + 90)
}
add_my_indicator <- newTA(FUN = my_indicator, preFUN = Cl, legend.name = "My Indicator", on = 1)
add_my_indicator()


pepsi <- getSymbols('PEP', from ='2015-01-01', to='2016-11-19', adjust = T, auto.assign = F)
coke <- getSymbols('COKE', from ='2015-01-01', to='2016-11-19', adjust = T, auto.assign = F)
prices <- cbind(pepsi[,6], coke[,6])
prices_changes <- apply(prices, 2, diff)
plot(prices_changes[,1], prices_changes[,2],
     xlab = 'Coke', ylab = 'Pepsi')
grid()
ans <- lm(prices_changes[,1]~prices_changes[,2])
beta <- ans$coefficients[2]
Coke <- 1000
Pepsi <- Coke/beta

ans2 <- lm(prices_changes[,2]~prices_changes[,1])
beta2 <- ans2$coefficients[2]

# Total Least Squares vs Ordinary Least Squares
plot(prices_changes[,1], prices_changes[,2],
     xlab = 'Coke', ylab = 'Pepsi')
abline(ans)
abline(ans2)
r <- prcomp(~prices_changes[,1] + prices_changes[,2])
slope <- r$rotation[2,1]/r$rotation[1,1]
intercept <- r$center[2] - slope * r$center[1]
abline(a = intercept, b = slope, lty = 3)


# simple spread strategy --------------------------------------------------
calculate_spread <- function(x, y, beta){
  return(y - beta * x)
}

calculate_beta_and_level <- function(x, y, start_date, end_date){
  require(xts)
  
  time_range <- paste(start_date, "::", end_date, sep = "")
  x <- x[time_range]
  y <- y[time_range]
  
  dx <- diff(x)
  dy <- diff(y)
  r <- prcomp(~ dx + dy)
  
  beta <- r$rotation[2,1]/r$rotation[1,1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  
  return(outL)
}

calculate_buy_sell_signals <- function(spread, beta, level, lower_threshold, upper_threshold){
  buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)
  
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")
  
  return(output)
}

start_date <- "2009-01-01"
end_date <- "2016-01-01"
x <- getSymbols('SPY', from =start_date, to=end_date, adjust = T, auto.assign = F)[,6]
y <- getSymbols('AAPL', from =start_date, to=end_date, adjust = T, auto.assign = F)[,6]

results <- calculate_beta_and_level(x, y, start_date, end_date)
results$beta
results$level

plot(results$spread, ylab = "Spread Value", main = "AAPL - beta * SPY",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# out of sample
start_date_out_sample <- "2012-01-01"
end_date_out_sample <- "2012-10-22"
x2 <- getSymbols('SPY', from =start_date_out_sample, to=end_date_out_sample, adjust = T, auto.assign = F)[,6]
y2 <- getSymbols('AAPL', from =start_date_out_sample, to=end_date_out_sample, adjust = T, auto.assign = F)[,6]
spread_out_of_sample <- calculate_spread(x2, y2, results$beta)

plot(spread_out_of_sample, main = "APPL - beta * SPY",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

# rolling beta
window_length <- 10
dF <- cbind(x2, y2)
names(dF) <- c("x", "y")

run_regression <- function(dF){
  return(coef(lm(y~x-1,data= as.data.frame(dF))))
}

rolling_beta <- function(z, width){
  rollapply(z, width = width, FUN = run_regression, by.column = FALSE, align = "right")
}

betas <- rolling_beta(diff(dF), 10)

data <- merge(betas, dF)
data$spread <- data$y - lag(betas, 1) * data$x

returns <- diff(dF)/dF
return_beta <- rolling_beta(returns, 10)
data$spreadR <- diff(data$y) / data$y - return_beta * diff(data$x) / data$x

tail(data)
plot(betas); plot(return_beta)

threshold <- sd(data$spread, na.rm = TRUE)
plot(data$spread, main = "AAPL vs. SPY In-Sample")
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)
