library(freedom)
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
## write.csv2(df, file = "~/Desktop/data.csv")
##
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
AR <- freedom::adjusted_risk(as.numeric(table(df$herd_urg)/nrow(df)), c(1, 2.3))
EPHI <- freedom::EffProbInf(0.02, AR)
EPHI <- EPHI[df$herd_urg[match(unique(df$ppn), df$ppn)]]
## Now calculate the EPAI for each animal category in each herd
df$epai <- unlist(
lapply(unique(df$ppn), function(x){
    df <- df[df$ppn == x,]
    prop <- df$N_animal_urg / sum(df$N_animal_urg)
    freedom::EffProbInf(0.15,freedom::adjusted_risk(prop, c(1, 3)))
})
)
df$ephi <- EPHI[df$herd_urg]
## Then use this in the Herd sensitivity Calculation
##
HSE <- do.call("c", lapply(unique(df$ppn), function(x){
    freedom::hse(df$n_animal_urg[df$ppn == x],
                 df$N_animal_urg[df$ppn == x],
                 0.70, df$epai[df$ppn == x])
}))
## Then the system sensitivity
system_sens <- sysse(EPHI, HSE)
## Posterior probability of freedom.
##
## This is calculated based on the prior probabiltiy of freedom and
## the sensitivity of the surveillance system.
post_pf <- post_fr(0.5, system_sens)
## Prior probability at next year assuming an annual risk of
## introduction of 0.05%
stopifnot(identical(round(prior_fr(post_pf, 0.05), 15), 0.642596871541303))
