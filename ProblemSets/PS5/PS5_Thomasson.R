library(rvest)
library(tidyverse)

# PS5 - NON-API

## Bring in Current NBA Draft Odds

url <- "https://www.tankathon.com/pick_odds"

## Select Desired Table

css_selector <- "#pick-distribution"

## Recreate Desired Table in R

webpage <- read_html(url)

Current_2024_NBA_Draft_Lottery_Odds <- webpage %>% html_node(css_selector) %>% html_table()

## Enjoy

view(Current_2024_NBA_Draft_Lottery_Odds)

# PS5 - API

## We use quantmod to bring in Yahoo Finance Data

library(quantmod)

## Select Disney ticker symbol
ticker_symbol <- "DIS"

## Download Daily Stock Price Data for the last year
getSymbols(ticker_symbol, src = "yahoo", from = Sys.Date() - 365, to = Sys.Date())

## Enjoy

view(DIS)