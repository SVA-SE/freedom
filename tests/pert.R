library(freedom)

ob <- rpert(10, 0.5, 1.5, 1)
stopifnot(length(ob) == 10)

## Min greater than max
ob <- tools::assertError(rpert(10, 1.5, 0.5, 1))[[1]]$message
stopifnot(length(grep("invalid parameters", ob)) == 1)

## special case with the min and max are the same
ob <- rpert(10, 1.5, 1.5, 1.5)
stopifnot(all(ob == 1.5))

## A check where mu == x.mode
ob <- rpert(10, 2, 4, 3, 4)
stopifnot(length(ob) ==10)

## acheck where mu != x.mode
ob <- rpert(10, 2, 4, 3.5, 4)
stopifnot(length(ob) ==10)
