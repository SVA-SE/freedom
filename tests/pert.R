library(freedom)

## Tolerance of the agreement between observed and expected
tol <- 1e-7

ob <- rpert(10, 0.5, 1.5, 1)
stopifnot(length(ob) == 10L)

## Min greater than max
ob <- tools::assertError(rpert(10, 1.5, 0.5, 1))[[1]]$message
stopifnot(length(grep("invalid parameters", ob)) == 1L)

## special case with the min and max are the same
ob <- rpert(10, 1.5, 1.5, 1.5)
stopifnot(all(abs(ob - 1.5)) < tol)

## A check where mu == x.mode
ob <- rpert(10, 2, 4, 3, 4)
stopifnot(length(ob) == 10L)

## acheck where mu != x.mode
ob <- rpert(10, 2, 4, 3.5, 4)
stopifnot(length(ob) == 10L)

## Another check that failed prior to the fix with floating point
## equivalence
ob <- rpert(10, 0.7, 0.9, 0.8)
stopifnot(all(is.finite(ob)))
