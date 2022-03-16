library(freedom)
## Expect the Herd sensitivity to be 1 if you test all animals with a
## perfect test.
stopifnot(hse(1, 100, 100, 1, 0.05)$HSe == 1)
stopifnot(hse(1, 100, 100, 1, 0.05)$method == "finite")

## Expect the infinite method to be used with a smal fraction tested
stopifnot(hse(1, 1, 100, 1, 0.05)$method == "infinite")

## And the opposite
stopifnot(hse(1, 90, 100, 1, 0.05)$method == "finite")
