hh.fit <- train %>% 
model(TSLM(qddemand ~  fourier("1 year", K=2)))
                 ME      RMSE        MAE        MPE     MAPE
Test set -0.0002005041 0.0936113 0.06821242 -0.8626392 6.808883

hh.fit <- train %>% 
model(TSLM(qddemand ~  fourier("1 year", K=3)))
                ME       RMSE       MAE        MPE     MAPE
Test set -0.0002855676 0.08843545 0.0627155 -0.7759022 6.251861

hh.fit <- train %>%
model(ARIMA(qddemand ~  fourier("1 year", K=3) + PDQ(0,0,0)))
              ME       RMSE        MAE      MPE     MAPE
Test set 4.3393e-05 0.08846804 0.06272621 -0.74828 6.251595

hh.fit <- train %>% 
model(TSLM(qddemand ~ month))
            ME       RMSE        MAE        MPE     MAPE
Test set -1.861675e-05 0.08065927 0.05862478 -0.6387853 5.825373

hh.fit <- train %>%
model(ARIMA(qddemand ~ month + PDQ(0,0,0)))
                ME       RMSE        MAE       MPE     MAPE
Test set -0.006823742 0.09220506 0.07235797 -1.534354 7.397622

hh.fit <- train %>%
model(TSLM(qddemand ~ ns(month, df=9)))
                ME       RMSE        MAE        MPE     MAPE
Test set -0.0004997851 0.08788748 0.06371606 -0.7877387 6.345404

hh.fit <- train %>%
model(ARIMA(qddemand ~ ns(month, df=9)))
In sqrt(diag(best$var.coef)) : NaNs produced

hh.fit <- train %>%
model(TSLM(qddemand ~ ns(temp1, df = 4)
+ fourier("1 year", K=3)))
               ME       RMSE        MAE       MPE     MAPE
Test set -0.01989075 0.07556813 0.05615665 -2.472667 5.751922

hh.fit <- train %>%
  model(TSLM(qddemand ~ fourier("1 year", K=2)
             + temp))
                ME       RMSE        MAE        MPE     MAPE
Test set 0.002485701 0.08645172 0.06510107 -0.5070797 6.474047









#########################################################
hh.fit <- train %>%
model(TSLM(qddemand ~  fourier("1 year", K=2)
+ holiday + workday
+ dew + hum + spd
+ temp + prevtemp1 + prevtemp2 + prevtemp3
+ prevtemp4 + prevtemp5 + prevtemp6
+ day1temp + day2temp + day3temp
+ day4temp + day5temp + day6temp
+ lastmax + lastmin + avetemp))
                  ME       RMSE        MAE        MPE     MAPE
Test set -0.0004791991 0.07121177 0.05313408 -0.5806308 5.301697

hh.fit <- train %>%
model(TSLM(qddemand ~  fourier("1 year", K=2)
+ holiday + workday
+ dew + hum + spd
+ temp + prevtemp1 + prevtemp2 + prevtemp3
+ prevtemp4 + prevtemp5 + prevtemp6
+ day1temp + day2temp + day3temp
+ day4temp + day5temp + day6temp
+ lastmax + lastmin + avetemp
+ temp*dew*hum*spd
+ fourier("1 year", K=2)*temp*dew*hum*spd))
              ME       RMSE        MAE        MPE     MAPE
Test set 0.002264803 0.06222897 0.04420341 -0.1864885 4.455182

model(TSLM(qddemand ~  month
+ holiday + workday
+ dew + hum + spd
+ temp + prevtemp1 + prevtemp2 + prevtemp3
+ prevtemp4 + prevtemp5 + prevtemp6
+ day1temp + day2temp + day3temp
+ day4temp + day5temp + day6temp
+ lastmax + lastmin + avetemp
+ temp*dew*hum*spd
+ month*temp*dew*hum*spd))
              ME       RMSE        MAE       MPE     MAPE
Test set 0.005163374 0.05064061 0.03704807 0.2864014 3.725914

hh.fit <- train %>%
model(TSLM(qddemand ~ month
+ day + holiday + workday
+ ns(temp, df = 5)
+ prevtemp1 + prevtemp2 + prevtemp3
+ day1temp))
              ME       RMSE        MAE       MPE     MAPE
Test set -0.01374358 0.04611318 0.03488563 -1.520872 3.542697


hh.fit <- train %>%
model(TSLM(qddemand ~ month
+ day + holiday + workday
+ temp + I(temp^2) + I(temp^3)
+ prevtemp1 + prevtemp2 + prevtemp3
+ day1temp))
              ME       RMSE        MAE       MPE     MAPE
Test set 0.004924215 0.04462992 0.03297281 0.2015217 3.294722


hh.fit <- train %>%
model(TSLM(qddemand ~ month
+ fourier("1 week", K=2) + holiday + workday
+ temp + I(temp^2) + I(temp^3)
+ prevtemp1 + prevtemp2 + prevtemp3
+ lastmin))
              ME       RMSE        MAE       MPE     MAPE
Test set 0.00482622 0.04470739 0.03288283 0.1913143 3.283684

hh.fit <- train %>%
model(TSLM(qddemand ~ month
+ day + holiday + workday
+ temp + I(temp^2) + I(temp^3)
+ prevtemp1 + prevtemp2 + prevtemp3
+ lastmin))
               ME       RMSE        MAE       MPE     MAPE
Test set 0.004820761 0.04460396 0.03282269 0.1925044 3.278821


hh.fit <- train %>%
model(TSLM(qddemand ~ month
+ fourier("1 week", K=2) + holiday + workday
+ temp + I(temp^2) + I(temp^3)
+ prevtemp1 + prevtemp2 + prevtemp3
+ lastmin + dloadfc))
               ME       RMSE        MAE      MPE     MAPE
Test set 0.01490616 0.04363981 0.03108954 1.265657 3.040427


