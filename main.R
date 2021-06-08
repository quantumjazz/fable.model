library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)
library(tseries)
library(splines)

#
-------

# Tsibble object and train and test data sets
timeseries <- maketemps(timeseries, 1, 24)
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


# Combine hourly and quarterly forecasts
qfit <- data.frame(qfcast$.mean)
qfit <- data.frame(bg.econ.fcast$qdemand)
colnames(qfit)[[1]] <- "qfcast"
qfit <- (rep(qfit$qfcast, c(2184, 2184, 2208, 2208)))

# total demand
demand <- fc$.mean*qfit

# accuracy
test <- test %>% arrange(dateyear)
forecast::accuracy(demand, test$demand)

###################################################

# Ex ante hourly models and fits
# temp sim and bootstrap

temp <- train %>% 
  filter(dateyear <= "2016-02-28" | dateyear >= "2016-03-01")

tempsim <- forecast::bld.mbb.bootstrap(temp$temp1, num = 100, block_size = 24)
tempsim <- sapply(tempsim, unlist)
tempsim <- matrix(tempsim, nrow = 8760)
tempsim <- rowMeans(tempsim)

# add February 29th
a <- matrix(c(2.743628481,2.897167395,2.548108447,2.445868521,
              2.292329607,2.292329607,1.991061474,1.991061474,
              2.642002526,3.238494468,4.238494468,5.238494468,
              6.136254543,6.5397626,7.33528275,7.437522675,
              7.29330163,7.139762715,4.936846992,2.346778716,
              1.440058825,0.141326842,-0.445847231,-1.196492057))

temp <- append(tempsim, a, after = 1417)
test <- test %>% arrange(dateyear)
test <- test %>% add_column(temp)
colnames(test)[25] <- "simtemp"
t
# new fit
hh.fit <- train %>% 
  model(TSLM(qddemand ~ ns(temp1, df = 4) 
             + lag(temp1, 2) 
             + fourier("1 year", K=2)))

#bootstrap
fc.boostrap <- generate(hh.fit, new_data = test, times = 10, bootstrap = TRUE, bootstrap_block_size = 7)
fc.boostrap <- fc.boostrap %>% arrange(dateyear)
hh.fit <- fc.boostrap %>% group_by(timeofday) %>%
summarise(mean = mean(.sim, na.rm = TRUE))
forecast::accuracy(hh.fit$mean, test$qddemand)



# Reorder fit and forecast
#fit <- fit %>%  augment()
#fit <- fit %>% arrange(dateyear)

#fc <- arrange(fc, datetime)


# Combine forecasts
qfit <- qfcast %>%
filter(.model == "auto") %>%
select(.mean)

qfit <- (rep(qfit$.mean, c(2184, 2184, 2208, 2208)))
demand <- hh.fit$mean*qfit
forecast::accuracy(demand, test$demand)


#####################################################################
# Separate hourly fits
temp <- train %>% features(qddemand, unitroot_kpss)
temp <- train %>% features(qddemand, unitroot_pp)
train %>% features(qddemand, unitroot_ndiffs)
temp <- train %>% features(qddemand, unitroot_nsdiffs)


#hh.fit <- train %>% filter(timeofday == "1") %>% model(
#mdl1 = ARIMA(qddemand ~ ns(timeofyear, df = 5))
#)

hh.fit %>% glance()
hh.fit %>% select(mdl1) %>% tidy()
hh.fit %>% select(mdl2) %>% tidy()

augment(hh.fit) %>% features(.innov, ljung_box, lag=21, dof=13)

fc <- forecast(hh.fit, new_data = test)
accuracy(fc, data = timeseries)

hh.fit %>% augment() %>% filter(.model == "mdl1") %>% autoplot(qddemand) + geom_line(aes(y = .fitted), color = "red")
