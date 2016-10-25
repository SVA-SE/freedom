library(freedom)
## 50 herds, 
df <- sample_data(nherds = 50,
                 mean_herd_size = 500,
                 n_herd_urg = 1,
                 herd_dist = c(1),
                 herd_samp_frac = 0.15,
                 herd_samp_dist = c(1),
                 n_animal_urg = 1,
                 animal_dist = c(1),
                 animal_samp_frac = 0.15,
                 animal_samp_dist = c(1),
                 seed = 1)
##
## Because there is only 1 unit risk group at the herd and animal
## level the Adjusted risks are 1 and the effective probability of
## infection is the same as the design prevalence. The Design
## prevalence for the calculation below is 2% at the herd level and
## 15% at the animal level. The sensitivity of the test is 70%. We
## will calculate the Sensitivity of the surveillance system.
##
## First the Herd sensitivity
##
## Test the time it takes...
## ptm <- proc.time()
## for(i in 1:1000){
df$hse <- do.call("c", lapply(df$ppn, function(x){
    freedom::hse(df$n_animal_urg[df$ppn == x],
                 df$N_animal_urg[df$ppn == x],
                 0.70, rep(0.15, nrow(df))[df$ppn == x])
}))
## Then the system sensitivity
system_sens <- sysse(rep(0.02, nrow(df)), df$hse)
## Posterior probability of freedom.
##
## This is calculated based on the prior probabiltiy of freedom and
## the sensitivity of the surveillance system.
post_pf <- post_fr(0.5, system_sens)
## Prior probability at next year assuming an annual risk of
## introduction of 0.05%
prior_fr(post_pf, 0.05)
## }
## proc.time()-ptm

## The time is a little slow 4 seconds. I think that improving the
## design of the Hse function to avoid appling the function over
## indexes of the dataframe would be a significant improvement. This
## could be done by reshaping the dataframe from long to wide. Then
## submitting each vector to the function. 
