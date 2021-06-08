#Hourly fits and forecasts
hh.fit1 <- train %>% filter(timeofday == "0") %>% model(
  h1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + day
               + ns(dew, df=2) + lastmaxtemp
               + pdq(1,0,0) + PDQ(0,0,0))) 
hh.fit1 %>% select(h1) %>% tidy()
augment(hh.fit1) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit1) %>% features(.innov, ljung_box, lag=28, dof=15)
fc1 <- forecast(hh.fit1, new_data = test)
accuracy(fc1, data = timeseries)

hh.fit2 <- train %>% filter(timeofday == "1") %>% model(
  h2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + day
               + ns(dew, df=2) + lastmaxtemp
               + pdq(1,0,0) + PDQ(0,0,0)))
hh.fit2 %>% select(h2) %>% tidy()
augment(hh.fit2) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit2) %>% features(.innov, ljung_box, lag=28, dof=15)
fc2 <- forecast(hh.fit2, new_data = test)
accuracy(fc2, data = timeseries)

hh.fit3 <- train %>% filter(timeofday == "2") %>% model(
  h3 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + day
               + ns(dew, df=2) + avetemp
               + pdq(1,0,0) + PDQ(0,0,0)))
hh.fit3 %>% select(h3) %>% tidy()
augment(hh.fit3) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit3) %>% features(.innov, ljung_box, lag=28, dof=15)
fc3 <- forecast(hh.fit3, new_data = test)
accuracy(fc3, data = timeseries)

hh.fit4 <- train %>% filter(timeofday == "3") %>% model(
  h4 = ARIMA(qddemand ~ 1
               + day
               + ns(temp, df=2) + avedew
               + pdq(1,0,0) + PDQ(0,0,0)))
hh.fit4 %>% select(h4) %>% tidy()
augment(hh.fit4) %>% features(.innov, ljung_box, lag=21, dof=11)
augment(hh.fit4) %>% features(.innov, ljung_box, lag=28, dof=11)
fc4 <- forecast(hh.fit4, new_data = test)
accuracy(fc4, data = timeseries)

hh.fit5 <- train %>% filter(timeofday == "4") %>% model(
  h5 = ARIMA(qddemand ~ 1
               + day
               + ns(temp, df=2) + avetemp
               + pdq(3,0,0) + PDQ(0,0,0)))
hh.fit5 %>% select(h5) %>% tidy()
augment(hh.fit5) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit5) %>% features(.innov, ljung_box, lag=28, dof=13)
fc5 <- forecast(hh.fit5, new_data = test)
accuracy(fc5, data = timeseries)

hh.fit6 <- train %>% filter(timeofday == "5") %>% model(
  h6 = ARIMA(qddemand ~ 1
               + day + holiday
               + avedew
               + pdq(1,0,0) + PDQ(0,0,0)))
hh.fit6 %>% select(h6) %>% tidy()
augment(hh.fit6) %>% features(.innov, ljung_box, lag=21, dof=12)
augment(hh.fit6) %>% features(.innov, ljung_box, lag=28, dof=12)
fc6 <- forecast(hh.fit6, new_data = test)
accuracy(fc6, data = timeseries)

hh.fit7 <- train %>% filter(timeofday == "6") %>% model(
  h7 = ARIMA(qddemand ~ 1
               + day + holiday
               + avedew
               + pdq(3,0,1) + PDQ(0,0,0)))
hh.fit7 %>% select(h7) %>% tidy()
augment(hh.fit7) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit7) %>% features(.innov, ljung_box, lag=28, dof=15)
fc7 <- forecast(hh.fit7, new_data = test)
accuracy(fc7, data = timeseries)

hh.fit8 <- train %>% filter(timeofday == "7") %>% model(
  h8 = ARIMA(qddemand ~ 1
               + day + holiday
               + avedew
               + pdq(3,0,1) + PDQ(0,0,0)))
hh.fit8 %>% select(h8) %>% tidy()
augment(hh.fit8) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit8) %>% features(.innov, ljung_box, lag=28, dof=15)
fc8 <- forecast(hh.fit8, new_data = test)
accuracy(fc8, data = timeseries)

hh.fit9 <- train %>% filter(timeofday == "8") %>% model(
  h9 = ARIMA(qddemand ~ 1
               + fourier("1 week", K=3) + holiday
               + avedew
               + pdq(3,0,1) + PDQ(0,0,0)))
hh.fit9 %>% select(h9) %>% tidy()
augment(hh.fit9) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit9) %>% features(.innov, ljung_box, lag=28, dof=15)
fc9 <- forecast(hh.fit9, new_data = test)
accuracy(fc9, data = timeseries)

hh.fit10 <- train %>% filter(timeofday == "9") %>% model(
h10 = ARIMA(qddemand ~ 1
+ day + holiday
+ avetemp
+ pdq(3,0,1) + PDQ(0,0,0)))
hh.fit10 %>% select(h10) %>% tidy()
augment(hh.fit10) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit10) %>% features(.innov, ljung_box, lag=28, dof=15)
fc10 <- forecast(hh.fit10, new_data = test)
accuracy(fc10, data = timeseries)

hh.fit11 <- train %>% filter(timeofday == "10") %>% model(
  h11 = ARIMA(qddemand ~ 1 + fourier("1 year", K=1)
               + workday
               + hdd + lastmindew
               + pdq(3,0,0) + PDQ(4,0,0)))
hh.fit11 %>% select(h11) %>% tidy()
augment(hh.fit11) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit11) %>% features(.innov, ljung_box, lag=28, dof=13)
fc11 <- forecast(hh.fit11, new_data = test)
accuracy(fc11, data = timeseries)

hh.fit12 <- train %>% filter(timeofday == "11") %>% model(
  h12 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd + lastmindew
               + pdq(3,0,0) + PDQ(3,0,0)))
hh.fit12 %>% select(h12) %>% tidy()
augment(hh.fit12) %>% features(.innov, ljung_box, lag=21, dof=14)
augment(hh.fit12) %>% features(.innov, ljung_box, lag=28, dof=14)
fc12 <- forecast(hh.fit12, new_data = test)
accuracy(fc12, data = timeseries)

hh.fit13 <- train %>% filter(timeofday == "12") %>% model(
  h13 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd + avetemp
               + pdq(3,0,0) + PDQ(2,0,0)))
hh.fit13 %>% select(h13) %>% tidy()
augment(hh.fit13) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit13) %>% features(.innov, ljung_box, lag=28, dof=13)
fc13 <- forecast(hh.fit13, new_data = test)
accuracy(fc13, data = timeseries)

hh.fit14 <- train %>% filter(timeofday == "13") %>% model(
  h14 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd + avetemp
               + pdq(3,0,0) + PDQ(2,0,0)))
hh.fit14 %>% select(h14) %>% tidy()
augment(hh.fit14) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit14) %>% features(.innov, ljung_box, lag=28, dof=13)
fc14 <- forecast(hh.fit14, new_data = test)
accuracy(fc14, data = timeseries)

hh.fit15 <- train %>% filter(timeofday == "14") %>% model(
  h15 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd + lastmaxtemp
               + pdq(3,0,0) + PDQ(2,0,0)))
hh.fit15 %>% select(h15) %>% tidy()
augment(hh.fit15) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit15) %>% features(.innov, ljung_box, lag=28, dof=13)
fc15 <- forecast(hh.fit15, new_data = test)
accuracy(fc15, data = timeseries)

hh.fit16 <- train %>% filter(timeofday == "15") %>% model(
  h16 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd + lastmaxtemp
               + pdq(3,0,0) + PDQ(2,0,0)))
hh.fit16 %>% select(h16) %>% tidy()
augment(hh.fit16) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit16) %>% features(.innov, ljung_box, lag=28, dof=13)
fc16 <- forecast(hh.fit16, new_data = test)
accuracy(fc16, data = timeseries)

hh.fit17 <- train %>% filter(timeofday == "16") %>% model(
  h17 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd + lastmaxtemp
               + pdq(3,0,0) + PDQ(2,0,0)))
hh.fit17 %>% select(h17) %>% tidy()
augment(hh.fit17) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit17) %>% features(.innov, ljung_box, lag=28, dof=13)
fc17 <- forecast(hh.fit17, new_data = test)
accuracy(fc17, data = timeseries)

hh.fit18 <- train %>% filter(timeofday == "17") %>% model(
  h18 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd
               + pdq(3,0,0) + PDQ(2,0,0)))
hh.fit18 %>% select(h18) %>% tidy()
augment(hh.fit18) %>% features(.innov, ljung_box, lag=21, dof=12)
augment(hh.fit18) %>% features(.innov, ljung_box, lag=28, dof=12)
fc18 <- forecast(hh.fit18, new_data = test)
accuracy(fc18, data = timeseries)

hh.fit19 <- train %>% filter(timeofday == "18") %>% model(
  h19 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday
               + hdd
               + pdq(2,0,0) + PDQ(2,0,0)))
hh.fit19 %>% select(h19) %>% tidy()
augment(hh.fit19) %>% features(.innov, ljung_box, lag=21, dof=11)
augment(hh.fit19) %>% features(.innov, ljung_box, lag=28, dof=11)
fc19 <- forecast(hh.fit19, new_data = test)
accuracy(fc19, data = timeseries)

hh.fit20 <- train %>% filter(timeofday == "19") %>% model(
  h20 = ARIMA(qddemand ~ 1
               + day
               + ns(dew, df=2) + avetemp + prevhdd5
               + pdq(3,0,2) + PDQ(0,0,0)))
hh.fit20 %>% select(h20) %>% tidy()
augment(hh.fit20) %>% features(.innov, ljung_box, lag=21, dof=16)
augment(hh.fit20) %>% features(.innov, ljung_box, lag=28, dof=16)
fc20 <- forecast(hh.fit20, new_data = test)
accuracy(fc20, data = timeseries)

hh.fit21 <- train %>% filter(timeofday == "20") %>% model(
  h21 = ARIMA(qddemand ~ 1
               + day + holiday
               + avetemp + prevhdd6
               + pdq(6,0,0) + PDQ(0,0,0), fixed = c(NA, NA, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)))
hh.fit21 %>% select(h21) %>% tidy()
augment(hh.fit21) %>% features(.innov, ljung_box, lag=21, dof=15)
augment(hh.fit21) %>% features(.innov, ljung_box, lag=28, dof=15)
fc21 <- forecast(hh.fit21, new_data = test)
accuracy(fc21, data = timeseries)

hh.fit22 <- train %>% filter(timeofday == "21") %>% model(
  h22 = ARIMA(qddemand ~ 1
               + day + holiday
               + prevhdd5
               + pdq(4,0,3, fixed = list(ma2=0)) + PDQ(0,0,0)))
hh.fit22 %>% select(h22) %>% tidy()
augment(hh.fit22) %>% features(.innov, ljung_box, lag=21, dof=17)
augment(hh.fit22) %>% features(.innov, ljung_box, lag=28, dof=17)
fc22 <- forecast(hh.fit22, new_data = test)
accuracy(fc22, data = timeseries)

hh.fit23 <- train %>% filter(timeofday == "22") %>% model(
  h23 = ARIMA(qddemand ~ 1
               + day + holiday
               + avetemp
               + pdq(2,0,1) + PDQ(0,0,0)))
hh.fit23 %>% select(h23) %>% tidy()
augment(hh.fit23) %>% features(.innov, ljung_box, lag=21, dof=14)
augment(hh.fit23) %>% features(.innov, ljung_box, lag=28, dof=14)
fc23 <- forecast(hh.fit23, new_data = test)
accuracy(fc23, data = timeseries)

hh.fit24 <- train %>% filter(timeofday == "23") %>% model(
  h24 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
               + workday + holiday
               + avetemp
               + pdq(2,0,1) + PDQ(0,0,0)))
hh.fit24 %>% select(h24) %>% tidy()
augment(hh.fit24) %>% features(.innov, ljung_box, lag=21, dof=13)
augment(hh.fit24) %>% features(.innov, ljung_box, lag=28, dof=13)
fc24 <- forecast(hh.fit24, new_data = test)
accuracy(fc24, data = timeseries)

fc <- bind_rows(fc1, fc2, fc3, fc4, fc5, fc6, fc7, fc8, fc9, fc10,
fc11, fc12, fc13, fc14, fc15, fc16, fc17, fc18, fc19, fc20,
fc21, fc22, fc23, fc24, .id = "idate")
forecast::accuracy(fc$.mean, test$qddemand)
fc <- fc %>% arrange(dateyear)

hhfits <- list(hh.fit1, hh.fit2, hh.fit3, hh.fit4, hh.fit5, hh.fit6, hh.fit7, hh.fit8, hh.fit9, hh.fit10,
hh.fit11, hh.fit12, hh.fit13, hh.fit14, hh.fit15, hh.fit16, hh.fit17, hh.fit18, hh.fit19, hh.fit20,
hh.fit21, hh.fit22, hh.fit23, hh.fit24)

hhfit <- lapply(hhfits, augfun)
hhfits <- hhfit %>% bind_rows()
hhfits <- hhfits %>% arrange(dateyear)
hhfits <- tsibble(hhfits, index = "dateyear", key = "timeofday")

