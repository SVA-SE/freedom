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
                 n_animal_urg = 2,
                 animal_dist = c(0.6, 0.4),
                 animal_samp_frac = 0.1,
                 animal_samp_dist = c(0.3, 0.7),
                 seed = 1)

## Now there are 2 herd level risk groups to use to calculate the HSe.
## The relative risk for Herd URG1 == 1 and URG2_relative_risk == 2.3
## Now there are 2 animal risk groups: URG1 RR == 1, URG2 the RR is
## 3. The Design prevalence for the calculation below is 2% at the
## herd level and 15% at the animal level. The sensitivity of the test
## is 70%. We will calculate the Sensitivity of the surveillance
## system. We will use the hypergeometric calculation throughout
##
## First calculate the Adjusted risks for each Herd unit risk group
##
##Since there are 2 rows for every herds the following is OK
table(df$herd_urg)/nrow(df)

## Now calculate the EPHI for each herdtype
AR <- adjusted_risk(as.numeric(table(df$herd_urg)/nrow(df)), c(1, 2.3))
EPHI <- EffProbInf(0.02, AR)
EPHI <- data.frame(ppn = sort(unique(df$ppn)),
                   EPHI = EPHI[df$herd_urg[match(sort(unique(df$ppn)), df$ppn)]])

## Now calculate the EPAI for each animal category in each herd
df$epai <- unlist(
lapply(unique(df$ppn), function(x){
    df <- df[df$ppn == x,]
    prop <- df$N_animal_urg / sum(df$N_animal_urg)
    EffProbInf(0.15, adjusted_risk(prop, c(1, 3)))
})
)
prop <- tapply(df$n_animal_urg, df$ppn, "sum") / tapply(df$N_animal_urg, df$ppn, "sum")
df$prop <- prop[match(df$ppn, names(prop))]

## Then use this in the Herd sensitivity Calculation
##
df_finite <- df[df$prop > 0.1,]
df_infinite <- df[df$prop <= 0.1,]
hse1 <- hse_finite(df_finite$ppn,
                   df_finite$n_animal_urg,
                   df_finite$N_animal_urg,
                   0.7,
                   df_finite$epai)
hse2 <- hse_infinite(df_infinite$ppn,
                     df_infinite$n_animal_urg,
                     0.7,
                     df_infinite$epai)
HSE <- rbind(hse1, hse2)
HSE$EPHI <- EPHI$EPHI[match(HSE$id, EPHI$ppn)]

## Then the system sensitivity
system_sens <- sysse(HSE$EPHI, HSE$HSe)

## Posterior probability of freedom.
##
## This is calculated based on the prior probability of freedom and
## the sensitivity of the surveillance system.
post_pf <- post_fr(0.5, system_sens)

## Prior probability at next year assuming an annual risk of
## introduction of 0.05%
stopifnot(all(abs(prior_fr(post_pf, 0.05) - 0.642596871541303) < tol))
rm(list = ls()[ls() != "tol"])

## 100 herds
df <- sample_data(nherds = 200,
                 mean_herd_size = 300,
                 n_herd_urg = 5,
                 herd_dist = c(0.4, 0.2, 0.2, 0.1, 0.1),
                 herd_samp_frac = 0.1,
                 herd_samp_dist = c(0.2, 0.3, 0.3, 0.1, 0.1),
                 n_animal_urg = 5,
                 animal_dist = c(0.5, 0.2, 0.1, 0.1, 0.1),
                 animal_samp_frac = 0.1,
                 animal_samp_dist = c(0.1, 0.3, 0.3, 0.1, 0.2),
                 seed = 1)
##
## Now there are 5 herd level risk groups to use to calculate the HSe.
## The relative risks are
## URG1 == 1
## URG2 == 2.3
## URG3 == 1.5
## URG4 == 9
## URG5 == 3
## Now there are 5 animal risk groups, the animal relative risks are:
## The relative risks are
## URG1 == 1
## URG2 == 3
## URG3 == 1.5
## URG4 == 2
## URG5 == 4
##
## The Design prevalence for the calculation below is 2% at the
## herd level and 15% at the animal level. The sensitivity of the test
## is 70%. We will calculate the Sensitivity of the surveillance
## system.
##
## First calculate the Adjusted risks for each Herd unit risk group
##
##Since there are 2 rows for every herds the following is OK
table(df$herd_urg)/nrow(df)

## Now calculate the EPHI for each herdtype
AR <- adjusted_risk(as.numeric(table(df$herd_urg)/nrow(df)), c(1, 2.3, 1.5, 9, 3))
EPHI <- EffProbInf(0.02, AR)
EPHI <- data.frame(ppn = sort(unique(df$ppn)),
                   EPHI = EPHI[df$herd_urg[match(sort(unique(df$ppn)), df$ppn)]])

## Now calculate the EPAI for each animal category in each herd
df$epai <- unlist(
lapply(unique(df$ppn), function(x){
    df <- df[df$ppn == x,]
    prop <- df$N_animal_urg / sum(df$N_animal_urg)
    EffProbInf(0.15, adjusted_risk(prop, c(1, 3, 1.5, 2, 4)))
})
)
prop <- tapply(df$n_animal_urg, df$ppn, "sum") / tapply(df$N_animal_urg, df$ppn, "sum")
df$prop <- prop[match(df$ppn, names(prop))]

## Then use this in the Herd sensitivity Calculation
##
df_finite <- df[df$prop > 0.1,]
df_infinite <- df[df$prop <= 0.1,]
hse1 <- hse_finite(df_finite$ppn, df_finite$n_animal_urg, df_finite$N_animal_urg, 0.7, df_finite$epai)
hse2 <- hse_infinite(df_infinite$ppn, df_infinite$n_animal_urg, 0.7, df_infinite$epai)
HSE <- rbind(hse1, hse2)
HSE$EPHI <- EPHI$EPHI[match(HSE$id, EPHI$ppn)]

## Then the system sensitivity
system_sens <- sysse(HSE$EPHI, HSE$HSe)

## Posterior probability of freedom.
##
## This is calculated based on the prior probability of freedom and
## the sensitivity of the surveillance system.
post_pf <- post_fr(0.5, system_sens)

## Prior probability at next year assuming an annual risk of
## introduction of 0.05%
stopifnot(all(abs(prior_fr(post_pf, 0.05) - 0.636108664339913) < tol))
