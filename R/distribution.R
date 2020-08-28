##' Sample a pert distribution
##'
##' Returns samples from a pert distribution
##'
##' @title rpert
##' @param n number of samples
##' @param x.min The minimum value in the sample
##' @param x.max The maximum value in the sample
##' @param x.mode The mode of the sample
##' @param lambda lambda
##' @import stats
##' @return a numeric vector of length n
##' @export
##' @examples
##' ## Generate 10000 samples from a pert distribution with a minimum
##' ## of 2, a max of 5, and a mode of 4.
##' samples <- rpert(10000, 2, 5, 4)
##' hist(samples)
##'
##' ## Generate a
rpert <- function(n,
                  x.min,
                  x.max,
                  x.mode,
                  lambda = 4) {

    if (x.min > x.max || x.mode > x.max || x.mode < x.min) {
        stop("invalid parameters")
    }

    x.range <- x.max - x.min
    if (x.range == 0) {
        return(rep(x.min, n))
    }

    mu <- (x.min + x.max + lambda * x.mode) / (lambda + 2)

    ## special case if mu == mode
    if (mu == x.mode) {
        v <- (lambda / 2) + 1
    }
    else {
        v <- ((mu - x.min) * (2 * x.mode - x.min - x.max)) /
            ((x.mode - mu) * (x.max - x.min))
    }

    w <- (v * (x.max - mu)) / (mu - x.min)
    return (rbeta(n, v, w) * x.range + x.min)
}
