library(freedom)

## Tolerance of the agreement between observed and expected
tol <- 1e-7

## 100 herds,
df <- sample_data(nherds = 100,
                 mean_herd_size = 300,
                 n_herd_urg = 2,
                 herd_dist = c(0.9, 0.1),
                 herd_samp_frac = 0.01,
                 herd_samp_dist = c(0.3, 0.7),
                 n_animal_urg = 1,
                 animal_dist = c(1),
                 animal_samp_frac = 0.05,
                 animal_samp_dist = c(1),
                 seed = 1)
##
## Now there are 2 herd level risk groups to use to calculate the HSe.
## The relative risk for Herd URG1 == 1 and URG2_relative_risk == 2.3
## We still only have a single Animal unit risk group. The Design
## prevalence for the calculation below is 2% at the herd level and
## 15% at the animal level. The sensitivity of the test is 70%. We
## will calculate the Sensitivity of the surveillance system. We will
## use the hypergeometric calculation throughout
##
## First calculate the Adjusted risks for each Herd unit risk group
##
table(df$herd_urg)/nrow(df)
AR <- adjusted_risk(as.numeric(table(df$herd_urg)/nrow(df)), c(1, 2.3))
EPHI <- EffProbInf(0.02, AR)
df$dp <- 0.15
df$ephi <- EPHI[df$herd_urg]

## Then use this in the Herd sensitivity Calculation
##
hse <- hse_finite(df$ppn, df$n_animal_urg, df$N_animal_urg, 0.7, df$dp)
df$hse <- hse$HSe[match(df$ppn, hse$id)]

## Then the system sensitivity
system_sens <- sysse(df$ephi, df$hse)

## Posterior probability of freedom.
##
## This is calculated based on the prior probability of freedom and
## the sensitivity of the surveillance system.
post_pf <- post_fr(0.5, system_sens)
## Prior probability at next year assuming an annual risk of
## introduction of 0.05%
stopifnot(all(abs(prior_fr(post_pf, 0.05) - 0.629618446231256) < tol))
