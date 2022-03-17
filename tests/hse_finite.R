## Tolerance of the agreement between observed and expected
tol <- 1e-7

library(freedom)

## expect hse to be less than 1 even though Se is 1 since you don't test all
ob <- hse_finite(1, 10, 20, 1, 0.1)$HSe
stopifnot(ob < 1)

## expect hse to be 1 when Se is 1 since you do test all
ob <- hse_finite(1, 20, 20, 1, 0.1)$HSe
stopifnot(abs(ob - 1) < tol)

## expect hse to be less than 1 since Se is 0.8 and the dp is low
ob <- hse_finite(1, 20, 20, 0.8, 0.01)$HSe
stopifnot(ob < 1)

## expect to get a data.frame with 2 rows
ob <- hse_finite(c(1,2), c(10, 10), c(20, 20), c(1, 1), c(0.1, 0.1))
stopifnot(nrow(ob) == 2L)

## can't test more than the population
ob <- tools::assertError(hse_finite(1, 25, 10, 0.8, 0.1))[[1]]$message
stopifnot(length(grep("One of the URG has more subunits tested than in the population", ob)) == 1)

## Vectors must be the same length
ob <- tools::assertError(hse_finite(c(1, 2), c(25, 25), 10, 0.8, 0.1))[[1]]$message
ex <- "The length of the n_tested vector must be equal to the N vector.\nie. you must describe both the number of animals tested in each\ngroup as well as how many animals are in each group."
stopifnot(length(grep(ex, ob)) == 1L)

## Allow for a single Se value for more groups
ob <- hse_finite(c(1, 2), c(10, 10), c(25, 25), 0.8, 0.1)
stopifnot(nrow(ob) == 2L)

## Ensure the id column is the same length as n_tested
ob <- tools::assertError(hse_finite(1, c(10, 10), c(25, 25), 0.8, 0.1))[[1]]$message
ex <- "Argument id \\(grouping variable\\) should be the same length as n_tested"
stopifnot(length(grep(ex, ob)) == 1L)


## check what happens when you use rounding
ob1 <- hse_finite(1, 1, 20, 1, 0.01, "round")$HSe
stopifnot(abs(ob1 - 0.05) < tol)

ob2 <- hse_finite(1, 1, 20, 1, 0.01, "floor")$HSe
ob3 <- hse_finite(1, 1, 20, 1, 0.01, "ceiling")$HSe
ob4 <- hse_finite(1, 1, 20, 1, 0.05)$HSe
stopifnot(identical(ob1, ob2, ob3, ob4))

## expect the floor method to be more conservative than the ceiling method
ob1 <- hse_finite(1, 1, 20, 1, 0.06, "ceiling")$HSe
ob2 <- hse_finite(1, 1, 20, 1, 0.06, "floor")$HSe
stopifnot(ob1 > ob2)

## expect the 'none' method to be more conservative than round for small fractions
ob1 <- hse_finite(1, 1, 20, 1, 0.01, "round")$HSe
ob2 <- hse_finite(1, 1, 20, 1, 0.01, "none")$HSe
stopifnot(ob1 > ob2)
