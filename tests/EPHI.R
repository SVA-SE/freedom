library(freedom)

## Tolerance of the agreement between observed and expected
tol <- 1e-7

## A working example
ob <- EffProbInf(0.05, 2)
stopifnot(abs(ob - 0.1) < tol)

## wrong length of args
ob <- tools::assertError(EffProbInf(c(0.05, 0.1, 0.3), c(2, 3)))[[1]]$message
ex <- paste("The design prevalence \\(dp\\) vector must be length",
            "1 or be equal in length to the AR vector")
stopifnot(length(grep(ex, ob)) == 1L)

## warning for unreasonable values
ob <- tools::assertWarning(EffProbInf(c(2, 0.1), c(2, 3)))[[1]]$message
ex <- paste("The EPI should not be greater than 1 for any URG.",
            "Consider your choices of design prevalence and",
            "therelative risks of the URG")
stopifnot(length(grep(ex, ob)) == 1L)
