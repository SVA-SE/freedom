## Tolerance of the agreement between observed and expected
tol <- 1e-7

library(freedom)
ob <- tools::assertError(adjusted_risk(1, 1))[[1]]$message
ex <- "We expect at least 2 unit risk groups \\(URG\\). ie the length of the vector arguments should be > 1"
stopifnot(length(grep(ex, ob)) == 1)

ob <- tools::assertError(adjusted_risk(c(1, 1), c(1, 2)))[[1]]$message
ex <- "The proportion vector must sum to 1 and each value between 0 and 1"
stopifnot(length(grep(ex, ob)) == 1)

ob <- tools::assertError(adjusted_risk(c(2, -1), c(1, 2)))[[1]]$message
ex <- "The proportion vector must sum to 1 and each value between 0 and 1"
stopifnot(length(grep(ex, ob)) == 1)

ob <- tools::assertError(adjusted_risk(c(0.5, 0.5), c(2, 1)))[[1]]$message
ex <- "The relative risk of the first URG must be 1. This is the referent group"
stopifnot(length(grep(ex, ob)) == 1)

ob <- tools::assertError(adjusted_risk(c(0.5), c(2, 1)))[[1]]$message
ex <- "The length of the proportions vector must be equal to the length of the RR vector"
stopifnot(length(grep(ex, ob)) == 1)

ob <- adjusted_risk(c(0.5, 0.5), c(1, 2))
ex <- c(2/3, 4/3)
stopifnot(all(abs(ob - ex) < tol))
