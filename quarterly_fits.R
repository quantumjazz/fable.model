# Test for unit roots and stationarity
adf.test(bg_econ$gdp, k=4)
bg.econ %>% features(gdp, unitroot_kpss)

# Quarterly model
# prepare data

bg_econ$quarter <- yearquarter(bg_econ$quarter)

bg_econ <- bg_econ %>%
  as_tsibble(index = quarter)

bg.econ <- bg_econ %>%
  filter(year < "2020")
bg.econ.fcast <- bg_econ %>%
  filter(year >= "2020")



quarterly.fit <- bg.econ %>%
model(mdl1 = ARIMA(qdemand ~ gdp + hddays + cddays + pdq(0, 0, 0) + PDQ(0, 1, 0)))

glance(quarterly.fit) %>% arrange(AICc)

tidy(quarterly.fit)
augment(quarterly.fit) %>% features(.resid, ljung_box, lag=8, dof=4)

qfcast <- forecast(quarterly.fit, new_data = bg.econ.fcast)
accuracy(qfcast, bg_econ)

qfit <- tibble(quarter = qfcast$quarter, qfcast = qfcast$.mean)
q.demand <- spline(qfit$quarter, qfit$qfcast, n = length(fc$datetime), method = "natural")
#q.demand <- (rep(qfit$.mean, c(2184, 2184, 2208, 2208)))
test <- test %>% arrange(dateyear)
demand <- fc$.mean*q.demand$y
df <- demand
demand <- tibble(datetime = fc$datetime,demand_fcast = df, demand = test$demand)

forecast::accuracy(demand$demand_fcast, test$demand)







