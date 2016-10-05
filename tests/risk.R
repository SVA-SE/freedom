library(freedom)
adjusted_risk(c(0.1, 0.9), c(1,4))
temp <- EffProbInf(dp = 0.01, adjusted_risk(c(0.1, 0.9), c(1,4)))
temp
hse(c(5,10), c(100, 110), 0.90, temp)
hse(c(5,10), c(10, 15), 0.90, temp)
