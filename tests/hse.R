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
problem <- 1
ex <- paste("Greater than 100% of animals cannot be tested.",
            "This occurs in the following ids:",
            paste(problem, collapse = ", "),
            "To ignore this an default to infinite population",
            "for these herds, set force = TRUE", sep = "\n")
ob <- length(grep(ex, res[[1]]$message)) == 1L
stopifnot(ob)

## and allow the same with force
hse(1, 100, 90, 0.9, dp = 0.1, force = TRUE)

## finite with multiple dps an Se
ob <- nrow(hse(c(1, 2, 3),
               c(10, 10, 10),
               c(20, 20, 20),
               c(0.8, 0.7, 0.8),
               c(0.1))) == 3L
stopifnot(ob)

ob <- nrow(hse(c(1, 2, 3),
               c(10, 10, 10),
               c(20, 20, 20),
               c(0.8),
               c(0.1, 0.4, 0.3))) == 3L
stopifnot(ob)

ob <- nrow(hse(c(1, 2, 3),
               c(2, 2, 2),
               c(50, 50, 50),
               c(0.8, 0.7, 0.8),
               c(0.1))) == 3L
stopifnot(ob)

ob <- nrow(hse(c(1, 2, 3),
               c(2, 2, 2),
               c(50, 50, 50),
               c(0.8),
               c(0.1, 0.4, 0.3))) == 3L
stopifnot(ob)
