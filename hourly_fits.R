#Hour 0, mdl 3
hh.fit <- train %>% filter(timeofday == "0") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + ns(timeofyear, df=5)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + hdd + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=3)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl3 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0))
)

#Hour 1, mdl 1
hh.fit <- train %>% filter(timeofday == "1") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + prevtemp2
+ pdq(1,0,0) + PDQ(0,0,0))
)

#Hour 2, mdl 2
hh.fit <- train %>% filter(timeofday == "2") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + avetemp
+ pdq(1,0,0) + PDQ(0,0,0))
)

#Hour 3, mdl 2
hh.fit <- train %>% filter(timeofday == "3") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3)
+ ns(temp, df=2) + avedew
+ pdq(1,0,0) + PDQ(0,0,0))
)

#Hour 4, mdl 2
hh.fit <- train %>% filter(timeofday == "4") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3)
+ ns(temp, df=2) + avetemp
+ pdq(3,0,0) + PDQ(0,0,0))
)

#Hour 5, mdl 2, change avetemp with avedew
hh.fit <- train %>% filter(timeofday == "5") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ fourier("1 week", K=3)
+ ns(dew, df=2) + lastmaxtemp
+ pdq(1,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3) + holiday
+ avedew
+ pdq(1,0,0) + PDQ(0,0,0))
)

#Hour 6, mdl 2, pdq(4,0,0) or pdq(3,0,1)
hh.fit <- train %>% filter(timeofday == "6") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + ns(timeofyear, df=9)
+ fourier("1 week", K=3) + workday + holiday
+ avetemp + ns(temp, df=2)
+ pdq(3,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3) + holiday
+ avedew
+ pdq(3,0,1) + PDQ(0,0,0), fixed = c(NA, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)))

#Hour 7, mdl 2
hh.fit <- train %>% filter(timeofday == "7") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + ns(timeofyear, df=9)
+ fourier("1 week", K=3) + workday + holiday
+ avetemp + ns(temp, df=2)
+ pdq(3,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3) + holiday
+ avedew
+ pdq(3,0,1) + PDQ(0,0,0)))

#Hour 8, mdl 2
hh.fit <- train %>% filter(timeofday == "8") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + ns(timeofyear, df=9)
+ fourier("1 week", K=3) + workday + holiday
+ avetemp + ns(temp, df=2)
+ pdq(3,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3) + holiday
+ avedew
+ pdq(3,0,1) + PDQ(0,0,0)))

#Hour 9/8/7, mdl 1, hour 7 hdd==avedew
hh.fit <- train %>% filter(timeofday == "9") %>% model(
mdl1 = ARIMA(qddemand ~ 1
+ day + holiday
+ hdd + lag(hdd)
+ pdq(3,0,0) + PDQ(0,0,0), fixed = c(NA, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=2)
+ dew*workday
+ pdq(4,0,0) + PDQ(0,0,0)))

#Hour 10, mdl 2
hh.fit <- train %>% filter(timeofday == "10") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + lastmindew
+ pdq(3,0,0) + PDQ(3,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=1)
+ workday
+ hdd + lastmindew
+ pdq(3,0,0) + PDQ(4,0,0)))

#Hour 11, mdl 1
hh.fit <- train %>% filter(timeofday == "11") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + lastmindew
+ pdq(3,0,0) + PDQ(3,0,0)),
  mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=1)
+ workday
+ hdd + lastmindew
+ pdq(3,0,0) + PDQ(4,0,0)))

#Hour 12, mdl 1
hh.fit <- train %>% filter(timeofday == "12") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + avetemp
+ pdq(3,0,0) + PDQ(2,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=1)
 + workday
+ hdd + avedew
+ pdq(3,0,0) + PDQ(2,0,0)))

#Hour 13, mdl 1
hh.fit <- train %>% filter(timeofday == "13") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + avetemp
+ pdq(3,0,0) + PDQ(2,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + avedew
+ pdq(3,0,0) + PDQ(2,0,0)))



#Hour 14, mdl 2
hh.fit <- train %>% filter(timeofday == "14") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + avetemp
+ pdq(3,0,0) + PDQ(2,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + lastmaxtemp
+ pdq(3,0,0) + PDQ(2,0,0))
)


#Hour 15, mdl 2
hh.fit <- train %>% filter(timeofday == "15") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + avetemp
+ pdq(3,0,0) + PDQ(2,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + lastmaxtemp
+ pdq(3,0,0) + PDQ(2,0,0))
)

#Hour 16, mdl 2
hh.fit <- train %>% filter(timeofday == "16") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + avetemp
+ pdq(3,0,0) + PDQ(2,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + lastmaxtemp
+ pdq(3,0,0) + PDQ(2,0,0))
)

#Hour 17, mdl 2 == after dropping vars with opposite/counterintuitive signs
hh.fit <- train %>% filter(timeofday == "17") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd + lastmintemp
+ pdq(3,0,0) + PDQ(2,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ ns(temp, df=2)
+ pdq(3,0,0) + PDQ(2,0,0))
)

#Hour 18, mdl 2
hh.fit <- train %>% filter(timeofday == "17") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ day
+ hdd
+ pdq(2,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday
+ hdd
+ pdq(2,0,0) + PDQ(2,0,0))
)

#Hour 19, mdl 2
hh.fit <- train %>% filter(timeofday == "19") %>% model(
mdl1 = ARIMA(qddemand ~ 1
+ day
+ ns(dew, df=2) + avetemp
+ pdq(3,0,2) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ fourier("1 week", K=3)
+ ns(dew, df=2) + avetemp + prevhdd5
+ pdq(3,0,2) + PDQ(0,0,0))
)


#Hour 20, mdl 2
hh.fit <- train %>% filter(timeofday == "20") %>% model(
mdl1 = ARIMA(qddemand ~ 1
+ day + holiday
+ avetemp + prevhdd6
+ pdq(2,0,0) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ day + holiday
+ avetemp + prevhdd6
+ pdq(6,0,0) + PDQ(0,0,0), fixed = c(NA, NA, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
)

#Hour 21, mdl2
hh.fit <- train %>% filter(timeofday == "21") %>% model(
mdl1 = ARIMA(qddemand ~ 1
+ day + holiday
+ avetemp
+ pdq(4,0,2) + PDQ(0,0,0)),
mdl2 = ARIMA(qddemand ~ 1
+ day + holiday
+ prevhdd5
+ pdq(4,0,3, fixed = list(ma2=0)) + PDQ(0,0,0)),
mdl3 = ARIMA(qddemand ~ 1
+ day
+ prevhdd5
+ pdq(5,0,2) + PDQ(0,0,0)),
)


#Hour 22, both mdl
hh.fit <- train %>% filter(timeofday == "22") %>% model(
mdl1 = ARIMA(qddemand ~ 1
+ day + holiday
+ avetemp
+ pdq(2,0,1) + PDQ(0,0,0)),
mdl2= ARIMA(qddemand ~ 1
+ day
+ avetemp
+ pdq(5,0,1) + PDQ(0,0,0))
)


#Hour 23, mdl 1
hh.fit <- train %>% filter(timeofday == "23") %>% model(
mdl1 = ARIMA(qddemand ~ 1 + fourier("1 year", K=2)
+ workday + holiday
+ avetemp
+ pdq(2,0,1) + PDQ(0,0,0)),
mdl2= ARIMA(qddemand ~ 1 + fourier("1 year", K=1)
+ day + holiday
+ avetemp
+ pdq(2,0,1) + PDQ(0,0,0)),
)
