library(freedom)

## Tolerance of the agreement between observed and expected
tol <- 1e-7

## Adjusted risks
stopifnot(any(abs(adjusted_risk(c(0.1, 0.9), c(1, 4)) - c(0.2702702703, 1.0810810811)) < tol))
## Effective probability of infection
stopifnot(any(abs(EffProbInf(dp = 0.01, adjusted_risk(c(0.1, 0.9), c(1, 4))) - c(0.0027027027, 0.0108108108)) < tol))

## Herd Se
temp <- EffProbInf(dp = 0.01, adjusted_risk(c(0.1, 0.9), c(1,4)))
stopifnot(any(abs(hse_infinite(c(1, 1), c(5, 10), 0.9, temp)$HSe - 0.1041217047) < tol))
stopifnot(any(abs(hse_finite(c(1, 1), c(5, 10), c(10, 15), 0.90, temp)$HSe - 0.1518904312) < tol))

## surveillance system Se

stopifnot(any(abs(sysse(dp = c(0.1, 0.2, 0.01, 0.005, 0.01),
                        hse = c(0.9, 0.5, 0.02, 0.99, 0.8)) - 0.1917353029) < tol))

## Temporal discounting
## Assuming the annual surveillance system sensitivity is 99%
Se <- 0.8
intro <- 0.02
year1post <- post_fr(0.1, Se)
year2prior <- prior_fr(year1post, intro)
year2post <- post_fr(year2prior, Se)

## Nest these to just calculate the next year (year 3) posterior
stopifnot(any(abs(post_fr(prior_fr(year2post, intro), Se) - 0.9260259179) < tol))
