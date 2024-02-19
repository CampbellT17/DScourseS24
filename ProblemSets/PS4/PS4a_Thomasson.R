#!/usr/bin/Rbatch

library(jsonlite)
library(httr)
library(tidyverse)

url <- "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"
response <- GET(url)
dates.json <- content(response, as = "text")

mylist <- fromJSON(dates.json)

mydf <- bind_rows(mylist$result[-1])

class(mydf)

class(mydf$date)

n <- 5
head(mydf, n)
