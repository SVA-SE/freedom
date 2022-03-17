library(freedom)

stopifnot(!freedom:::valid_proportions(c(0.4, 0.3)))
stopifnot(freedom:::valid_proportions(c(0.7, 0.3)))
stopifnot(freedom:::valid_proportions(c(0.7, 0.2, 0.1)))
stopifnot(freedom:::valid_proportions(1))
stopifnot(!freedom:::valid_proportions(-1))
stopifnot(!freedom:::valid_proportions(2))
