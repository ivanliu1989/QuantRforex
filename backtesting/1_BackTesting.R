# https://timtrice.github.io/backtesting-strategies/index.html
# http://www.r-programming.org/papers
# https://docs.google.com/presentation/d/1fGzDc-LFfCQJKHHzaonspuX1_TTm1EB5hlvCEDsz7zw/pub?slide=id.p
library(quantstrat)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(lattice)
library(pander)
library(tidyr)
library(webshot)

sessionInfo()

# BTO: Buy to Open (open long positions)
# BTC: Buy to close (close short positions)
# SL: Stop-limit order
# STO: Sell to open (open short positions)
# STC: Sell to close (close long positions)
# TS: Trailing-stop order
