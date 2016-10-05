library(freedom)

## Adjusted risks
adjusted_risk(c(0.1, 0.9), c(1,4))

## Effective propbabiltyi of infection
temp <- EffProbInf(dp = 0.01, adjusted_risk(c(0.1, 0.9), c(1,4)))
temp

## Herd Se
hse(c(5,10), c(100, 110), 0.90, temp)
hse(c(5,10), c(10, 15), 0.90, temp)

## surveillance system Se

sysse(dp = c(0.1, 0.2, 0.01, 0.005, 0.01),
      hse = c(0.9, 0.5, 0.02, 0.99, 0.8))
