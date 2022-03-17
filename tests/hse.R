library(freedom)

## Tolerance of the agreement between observed and expected
tol <- 1e-7

## Expect the Herd sensitivity to be 1 if you test all animals with a
## perfect test.
stopifnot(hse(1, 100, 100, 1, 0.05)$HSe == 1)
stopifnot(hse(1, 100, 100, 1, 0.05)$method == "finite")

## Expect the infinite method to be used with a smal fraction tested
stopifnot(hse(1, 1, 100, 1, 0.05)$method == "infinite")

## And the opposite
stopifnot(hse(1, 90, 100, 1, 0.05)$method == "finite")

## Makesure we get an error when oversampling
res <- tools::assertError(hse(1, 100, 90, 0.9, dp = 0.1))

ob <- length(grep("Greater than 100% of animals cannot be tested.\nThis occurs in the following ids:\n1\nTo ignore this an default to infinite population\nfor these herds, set force = TRUE", res[[1]]$message) == 1L)

stopifnot(abs(ob - 1) < tol)

## and allow the same with force
hse(1, 100, 90, 0.9, dp = 0.1, force = TRUE)
