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



SPY <- getSymbols('SPY', adjust = T, auto.assign = F)
AAPL <- getSymbols('AAPL', adjust = T, auto.assign = F)


# In-sample --------------------------------------------------------------
start_date <- "2009-01-01"
end_date <- "2011-12-31"
range <- paste(start_date , "::", end_date, sep = "")

x <- SPY[range, 6]
y <- AAPL[range, 6]
dF <- cbind(x, y)
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



# Rolling window ----------------------------------------------------------
window_length <- 10


# Time range --------------------------------------------------------------
start_date <- "2012-01-01"
end_date <- "2013-12-31"
range <- paste(start_date , "::", end_date, sep = "")


# Stock pair --------------------------------------------------------------
x <- SPY[range, 6]
y <- AAPL[range, 6]


# Bind these together into a matrix ---------------------------------------
dF <- cbind(x, y)
names(dF) <- c('x', 'y')


# Calculate rolling beta --------------------------------------------------
beta_out_of_sample <- rolling_beta(diff(dF), 10)


# Buy and Sell threshold --------------------------------------------------
data_out <- merge(beta_out_of_sample, dF)
data_out$spread <- data_out$y - lag(beta_out_of_sample, 1) * data_out$x


# Plot spread -------------------------------------------------------------
plot(data_out$spread, main = "AAPL vs. SPY out of sample")
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)



# Generate sell and buy signals -------------------------------------------
buys <- ifelse(data_out$spread > threshold, 1, 0)
sells <- ifelse(data_out$spread < -threshold, -1, 0)
data_out$signal <- buys + sells

plot(data_out$spread, main = "AAPL vs. SPY out of sample")
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

point_type <- rep(NA, nrow(data_out))
buy_index <- which(data_out$signal == 1)
sell_index <- which(data_out$signal == -1)

point_type[buy_index] <- 21
point_type[sell_index] <- 24
points(data_out$spread, pch = point_type)


# Summary -----------------------------------------------------------------
num_of_buy_signals <- sum(buys, na.rm = TRUE)
num_of_sell_signals <- sum(abs(sells), na.rm = TRUE)
num_of_buy_signals
num_of_sell_signals


# Trading the spread ------------------------------------------------------
prev_x_qty <- 0
position <- 0
trade_size <- 100
signal <- as.numeric(data_out$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_out$beta_out_of_sample)

qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for(i in 1:length(signal)){
  if(signal[i] == 1 && position == 0){
    # buy the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- -prev_x_qty
    qty_y[i] <- trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position == 0){
    # sell the spread
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- prev_x_qty
    qty_y[i] <- -trade_size
    position <- -1
  }
  
  if(signal[i] == 1 && position == -1){
    # currently short the spread and need to buy
    qty_x[i] <- -(round(beta[i] * trade_size) + prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- 2 * trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position == 1){
    # currently long the spread and need to sell
    qty_x[i] <- round(beta[i] * trade_size) + prev_x_qty
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- -2 * trade_size
    position <- -1
  }
}

qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)

data_out$qty_x <- qty_x
data_out$qty_y <- qty_y

tail(data_out)


# computing the equity curve ----------------------------------------------
compute_equity_curve <- function(qty, price){
  cash_buy <- ifelse(sign(qty) == 1, qty*price, 0)
  cash_sell <- ifelse(sign(qty) == -1, -qty*price, 0)
  position <- cumsum(qty)
  cumulative_buy <- cumsum(cash_buy)
  cumulative_sell <- cumsum(cash_sell)
  
  equity <- cumulative_sell - cumulative_buy + position * price
  return(equity)
}


# Add the equity curve columns to the data_out table ----------------------
data_out$equity_curve_x <- compute_equity_curve(data_out$qty_x, data_out$x)
data_out$equity_curve_y <- compute_equity_curve(data_out$qty_y, data_out$y)

plot(data_out$equity_curve_x + data_out$equity_curve_y, type = 'l')
