library(freedom)

## Tolerance of the agreement between observed and expected
tol <- 1e-7

## Expect to get back the single Se value if the dp is 1
stopifnot(abs(sysse(1, 0.8) - 0.8) < tol)

## if you test multiple herds you increase the cummulative Se (System Se)
stopifnot(sysse(c(1, 1), c(0.8, 0.8)) > 0.8)

## Expect the vectors to have the same length
ob <- tools::assertError(sysse(c(1, 1), c(0.8)))[[1]]$message
ex <- "The herd Se vector \\(hse\\) must be the same length\nas the Effective probability of infection of the herd \\(dp\\)"
stopifnot(length(grep(ex, ob)) == 1L)

## Expect the dp values to be between 0 and 1
ob1 <- tools::assertError(sysse(1, 1.5))[[1]]$message
ob2 <- tools::assertError(sysse(1, -0.1))[[1]]$message
ex <- "At least one of the hse values is greater than 1 or less than 0"
stopifnot(length(grep(ex, ob1)) == 1L)
stopifnot(length(grep(ex, ob2)) == 1L)
