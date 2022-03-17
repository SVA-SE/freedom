library(freedom)

## expect hse to be less than 1 even though Se is 1 since you don't test all
ob1 <- hse_infinite(1, 10, 1, 0.1)$HSe
stopifnot(ob1 < 1)

## expect hse to be 1 when Se is 1 since you do test all
ob2 <- hse_infinite(1, 20, 1, 0.1)$HSe
stopifnot(ob2 > ob1)

## expect hse to be less than 1 since Se is 0.8 and the dp is low
ob3 <- hse_infinite(1, 20, 1, 0.01)$HSe
stopifnot(ob3 < ob2)

## expect to get a data.frame with 2 rows
ob <- hse_infinite(c(1, 2), c(10, 10), c(1, 1), c(0.1, 0.1))
stopifnot(nrow(ob) == 2L)

## Vectors must be the same length
ob <- tools::assertError(hse_infinite(c(1, 2),
                                      c(10, 10),
                                      0.8,
                                      c(0.1, 0.2, 0.1)))[[1]]$message
ex <- paste("The length of the n_tested vector must be",
                   "equal to the dp vector. ie. you must describe",
                   "both the number of animals tested in each group",
                   "as well as the dp in each group.", sep = "\n")
stopifnot(length(grep(ex, ob)) == 1L)

## Vectors must be the same length
ob <- tools::assertError(hse_infinite(c(1, 2),
                                      c(10, 10),
                                      c(0.8, 0.7, 0.8),
                                      c(0.1, 0.2)))[[1]]$message
ex <- "Length of test_Se must be 1 or the length of n_tested"
stopifnot(length(grep(ex, ob)) == 1L)

## Allow for a single Se value for more groups
ob <- hse_infinite(c(1, 2), c(10, 10), 0.8, 0.1)
stopifnot(nrow(ob) == 2L)

## Ensure the id column is the same length as n_tested
ob <- tools::assertError(hse_infinite(1, c(10, 10), 0.8, 0.1))[[1]]$message
ex <- paste("Argument id \\(grouping variable\\) should be the",
            "same length as n_tested")
stopifnot(length(grep(ex, ob)) == 1L)
