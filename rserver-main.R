library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(tseries)
library(splines)


# Tsibble object and train and test data sets
timeseries$timeofday <- as.factor(timeseries$timeofday)
timeseries$month <- as.factor(timeseries$month)
timeseries$date <- as.factor(timeseries$date)

timeseries <- timeseries %>%
  mutate(datetime = as_datetime(datetime)) %>%
  mutate(dateyear = as_date(dateyear)) %>%
  as_tsibble(index = dateyear, key = timeofday)

train <- timeseries %>%
  filter(dateyear < "2020-01-01")

test <- timeseries %>%
  filter(dateyear >= "2020-01-01")
