# Separate hourly fits
train %>% filter(timeofday == "0") %>% features(demand, unitroot_kpss)
train %>% filter(timeofday == "0") %>% features(demand, unitroot_ndiffs)
train %>% filter(timeofday == "0") %>% features(demand, unitroot_nsdiffs)


hh.fit <- train %>% filter(timeofday == "0") %>% model(
  mdl1 = ARIMA(demand ~ ns(timeofyear, df = 5))
)

hh.fit %>% report()
hh.fit %>% select(mdl1) %>%  report()

augment(hh.fit) %>% features(.resid, ljung_box, lag=24, dof=18)

fc <- forecast(hh.fit, new_data = test)
accuracy(fc, data = timeseries)

hh.fit %>% augment() %>% filter(.model == "mdl1") %>% autoplot(qddemand) + geom_line(aes(y = .fitted), color = "red")
