library(freedom)

## expect hse to be less than 1 even though Se is 1 since you don't test all
ob <- hse_finite(1, 10, 20, 1, 0.1)$Hse
stopifnot(ob < 1)

## expect hse to be 1 when Se is 1 since you do test all
ob <- hse_finite(1, 20, 20, 1, 0.1)$HSe
stopifnot(ob == 1)

## expect hse to be less than 1 since Se is 0.8 and the dp is low
ob <- hse_finite(1, 20, 20, 0.8, 0.01)$HSe
stopifnot(ob < 1)

## expect to get a data.frame with 2 rows
ob <- hse_finite(c(1,2), c(10, 10), c(20, 20), c(1, 1), c(0.1, 0.1))
stopifnot(nrow(ob) == 2)

## can't test more than the population
ob <- tools::assertError(hse_finite(1, 25, 10, 0.8, 0.1))[[1]]$message
stopifnot(length(grep("One of the URG has more subunits tested than in the population", ob)) == 1)

## Vectors must be the same length
ob <- tools::assertError(hse_finite(c(1, 2), c(25, 25), 10, 0.8, 0.1))[[1]]$message
ex <- "The length of the n_tested vector must be equal to the N vector.\nie. you must describe both the number of animals tested in each\ngroup as well as how many animals are in each group."
stopifnot(length(grep(ex, ob)) == 1)

## Allow for a single Se value for more groups
ob <- hse_finite(c(1, 2), c(10, 10), c(25, 25), 0.8, 0.1)
stopifnot(nrow(ob) == 2)

## Ensure the id column is the same length as n_tested
ob <- tools::assertError(hse_finite(1, c(10, 10), c(25, 25), 0.8, 0.1))
ex <- "Argument id (grouping variable) should be the same length as n_tested"
stopifnot(length(grep(ex, ob) == 1))
