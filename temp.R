temp <- train %>% 
  filter(dateyear <= "2016-02-28" | dateyear >= "2016-03-01")

tempsim <- forecast::bld.mbb.bootstrap(temp$temp1, num = 100, block_size = 24)
tempsim <- sapply(tempsim, unlist)
tempsim <- matrix(tempsim, nrow = 8760)
tempsim <- rowMeans(tempsim)

# add february 29th
a <- matrix(c(6.631599,
6.733649,
6.385545,
6.385545,
6.385545,
6.385545,
6.087687,
6.139491,
6.297789,
6.841701,
7.195739,
7.441793,
7.758391,
7.600092,
7.302234,
7.910756,
8.054760,
7.948265,
8.000068,
7.297858,
7.437417,
6.139491,
6.191294,
6.647451
))

temp <- append(tempsim, a, after = 1417)
test <- test %>% arrange(dateyear)
test <- test %>% add_column(temp)
