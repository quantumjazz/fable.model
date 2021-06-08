#####
#Create temperature simulation
#2020
sim <- timeseries %>% filter(dateyear >= "2020-01-01") %>% select(temp, dew) #run only for 2020

sim <- timeseries %>% filter(dateyear >= "2018-01-01" & dateyear < "2019-01-01") %>% select(temp, dew)


#load test data
test <- read_excel("~/Documents/Market for Electricity/NewData/test.xlsx")

# add February 29th
temp <- train %>% filter(dateyear == "2016-02-29") %>% select(temp, dew)
sim <- sim %>% arrange(dateyear) #run for 2020 and 2016 too
sim <- add_row(sim, temp, .after = 1416)

#temperature bootstrap
tempsim <- forecast::bld.mbb.bootstrap(sim$temp, num = 2, block_size = 24)
dewsim <- forecast::bld.mbb.bootstrap(sim$dew, num = 2, block_size = 24)
test <- test %>% mutate(temp1 = tempsim[[2]], dew = dewsim[[2]])

#create test set
test <- maketemps(test, 1, 24)
test$timeofday <- as.factor(test$timeofday)
test$month <- as.factor(test$month)
test$date <- as.factor(test$date)
test <- test %>%
  mutate(datetime = as_datetime(datetime)) %>%
  mutate(dateyear = as_date(dateyear)) %>%
  as_tsibble(index = dateyear, key = timeofday)
