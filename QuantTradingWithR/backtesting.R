library(quantmod)
library(quantstrat)

options("getSymbols.warning4.0" = FALSE)
rm(list = ls(.blotter), envir = .blotter)


# Set the currency and the time zone --------------------------------------
currency("USD")
Sys.setenv(TZ = "UTC")


# Define symbols of interest ----------------------------------------------
symbols <- c("XLB", "XLE", "XLF", "XLP", "XLI", "XLU", "XLV", "XLK", "XLY", "RWR", "EWJ", "EWG"
             , "EWU", "EWC", "EWY", "EWA", "EWH", "EWS", "IYZ", "EZU", "IYR", "EWT", "EWZ", "EFA"
             , "IGE", "EPP", "LQD", "SHY", "IEF", "TLT")


# SPDR ETFs first, iShares ETFs afterwards --------------------------------
from = "2012-01-01"
to = "2016-11-01"
if(!"XLB" %in% ls()){
    suppressMessages(getSymbols(symbols, from = from, to = to, src = "yahoo", adjust = TRUE))
}


# Define the instrument type ----------------------------------------------
stock(symbols, currency = "USD", multiplier = 1)

