library(freedom)

ob <- tools::assertError(sample_data(herd_dist = c(0.7, 0.2, 0.1), n_herd_urg = 2))[[1]]$message
ex <- "The length of the herd distribution vector must be equal to the number of herd unit risk groups"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(herd_samp_dist = c(0.7, 0.2, 0.1), n_herd_urg = 2))[[1]]$message
ex <- "The length of the herd sample distribution vector must be equal to the number of herd unit risk groups"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(animal_dist = c(0.1, 0.2, 0.7), n_animal_urg = 2))[[1]]$message
ex <- "The length of the animal distribution vector must be equal to the number of animal unit risk groups"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(animal_samp_dist = c(0.1, 0.2, 0.7), n_animal_urg = 2))[[1]]$message
ex <- "The length of the animal sample distribution vector must be equal to the number of animal unit risk groups"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(herd_dist = c(0.4, 0.7)))[[1]]$message
ex <- "The distribution of herds between the herd unit risk groups must be between 0 and 1 and must sum to 1"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(herd_samp_dist = c(0.4, 0.7)))[[1]]$message
ex <- "The distribution of herd SAMPLES between the herd unit risk groups must be between 0 and 1 and must sum to 1"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(animal_dist = c(0.4, 0.7)))[[1]]$message
ex <- "The distribution of animals between the animal unit risk groups must be between 0 and 1 and sum to 1"
stopifnot(length(grep(ex, ob)) == 1L)

ob <- tools::assertError(sample_data(animal_samp_dist = c(0.4, 0.7)))[[1]]$message
ex <- "The distribution of animal SAMPLES between the animal unit risk groups must be between 0 and 1 and sum to 1"
stopifnot(length(grep(ex, ob)) == 1L)
