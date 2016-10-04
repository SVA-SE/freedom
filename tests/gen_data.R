library(freedom)
df <- sample_data(nherds = 50,
                 mean_herd_size = 500,
                 n_herd_urg = 2,
                 herd_dist = c(0.8,0.2),
                 herd_samp_frac = 0.15,
                 herd_samp_dist = c(0.1, 0.9),
                 n_animal_urg = 2,
                 animal_dist = c(0.1, 0.9),
                 animal_samp_frac = 0.15,
                 animal_samp_dist = c(0.9, 0.1))

df <- sample_data(nherds = 10,
                 mean_herd_size = 500,
                 n_herd_urg = 2,
                 herd_dist = c(0.8,0.2),
                 herd_samp_frac = 0.15,
                 herd_samp_dist = c(0.1, 0.9),
                 n_animal_urg = 1,
                 animal_dist = c(1),
                 animal_samp_frac = 0.15,
                 animal_samp_dist = c(1))

df <- sample_data(nherds = 10,
                 mean_herd_size = 500,
                 n_herd_urg = 4,
                 herd_dist = c(0.1,0.2,0.6,0.1),
                 herd_samp_frac = 1,
                 herd_samp_dist = c(0.1, 0.8, 0.05, 0.05),
                 n_animal_urg = 4,
                 animal_dist = c(0.05, 0.05,0.1, 0.8),
                 animal_samp_frac = 0.15,
                 animal_samp_dist = c(0.8,0.05,0.05,0.1))
