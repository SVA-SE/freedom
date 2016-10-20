library(freedom)

## Adjusted risks
stopifnot(identical(round(adjusted_risk(c(0.1, 0.9), c(1,4)), 10), c(0.2777777778, 40)))

## Effective propbabiltyi of infection
stopifnot(identical(round(EffProbInf(dp = 0.01, adjusted_risk(c(0.1, 0.9), c(1,4))), 10), c(0.0027777778, 0.4)))

## Herd Se
temp <- EffProbInf(dp = 0.01, adjusted_risk(c(0.1, 0.9), c(1,4)))
stopifnot(identical(round(hse(c(5,10), c(100, 110), 0.90, temp), 10), 0.9886141814))
stopifnot(identical(round(hse(c(5,10), c(10, 15), 0.90, temp), 10), 0.9959714589))

## surveillance system Se

stopifnot(identical(round(sysse(dp = c(0.1, 0.2, 0.01, 0.005, 0.01),
      hse = c(0.9, 0.5, 0.02, 0.99, 0.8)), 10), 0.1917353029))

## Temporal discounting
## Assuming the annual surveillance system sensitivity is 99%
Se <- 0.8
intro <- 0.02
year1post <- post_fr(0.1, Se)
year2prior <- prior_fr(year1post, intro)
year2post <- post_fr(year2prior, Se)
## Nest these to just calculate the next year (year 3) posterior
stopifnot(identical(round(post_fr(prior_fr(year2post, intro), Se),10), 0.9260259179))