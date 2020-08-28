##' Herd Sensitivity calculated with the assumption of a finite
##' population
##'
##' Calculate the Herd sensitivity when multiple samples from
##' individual units within the herd. The function uses the total
##' population size to adjust the estimates consistent with a finite
##' population. This method for calculation of HSe is typically used
##' when greater than 10% of the population has been sampled
##'
##' @title hse_finite
##' @param id The herdid.
##' @param n_tested The number tested in each URG
##' @param N The number of units in each of the URG
##' @param test_Se The sensitivity of the test. This may have length
##'     == 1 if all URG and all herds have the same test_Se. It may
##'     also have length(test_Se) == length(n_tested).
##' @param dp The design prevalance (dp) could be length(dp) == 1 if
##'     all URG and herds have the same dp. It could alternatively be
##'     length(dp) == length(n_tested) if different design prevalances
##'     are to be applied to each URG.
##' @return A data.frame. A dataframe is returned with 2 columns: "id" and HSe
##' @export
##' @examples
##' df <- data.frame(id = seq(1:20),
##'                  n_tested = rpois(20, 5),
##'                  N = 100,
##'                  test_Se = 0.3,
##'                  dp = 0.05)
##' ## Calculate the herd level sensitivity for each of these herds
##' hse_finite(df$id,
##'            df$n_tested,
##'            df$N,
##'            df$test_Se,
##'            df$dp)
hse_finite <- function(id,
                       n_tested,
                       N,
                       test_Se,
                       dp) {

    if (length(n_tested) != length(N)) {
        stop(paste("The length of the n_tested vector must be equal to the N vector.",
                   "ie. you must describe both the number of animals tested in each",
                   "group as well as how many animals are in each group.",
                   sep = "\n"))
    }

    if (any(n_tested > N)) {
        stop("One of the URG has more subunits tested than in the population")
    }

    if (!(length(dp) == 1 | length(n_tested) == length(dp))) {
        stop(paste("The length of the n_tested vector must be equal to the dp vector.",
                   "ie. you must describe both the number of animals tested in each",
                   "group as well as the dp in each group.", sep = "\n"))
    }

    if(!(length(test_Se) == 1 | length(test_Se) == length(n_tested))) {
        stop("The length of test_Se must be either 1 or the length of n_tested")
    }

    A <- 1 - (n_tested * test_Se / N)

    B <- dp * N

    df <- as.data.frame(1 - tapply(A ^ B, INDEX = id, FUN = "prod"))

    names(df) <- c("HSe")

    df$id <- rownames(df)

    df[,c("id", "HSe")]
}

##' Herd Sensitivity calculated with the assumption of an infinite
##' population
##'
##' Calculate the Herd sensitivity when multiple samples from
##' individual units within the herd. The function does not use the
##' population size to adjust the estimate. This is consistent with
##' the assumption of an infinite population size and is generally
##' used when less than 10% of the population has been sampled.
##'
##' @title hse_infinite
##' @param id The herdid
##' @param n_tested The number tested in each URG
##' @param test_Se The sensitivity of the test. This may have length
##'     == 1 if all URG and all herds have the same test_Se. It may
##'     also have length(test_Se) == length(n_tested).
##' @param dp The design prevalance (dp) could be length(dp) == 1 if
##'     all URG and herds have the same dp. It could alternatively be
##'     length(dp) == length(n_tested) if diff
##' @return A data.frame. A dataframe is returned with 2 columns: "id" and HSe
##' @export
##' @examples
##' df <- data.frame(id = seq(1:20),
##'                  n_tested = rpois(20, 5),
##'                  test_Se = 0.3,
##'                  dp = 0.05)
##'
##' ## Calculate the herd level sensitivity for each of these herds given
##' ## the assumption that the herds have an infintite size.
##' hse_infinite(df$id,
##'              df$n_tested,
##'              df$test_Se,
##'              df$dp)
hse_infinite <- function(id,
                         n_tested,
                         test_Se,
                         dp) {

    if (!(length(n_tested) == length(dp) | length(dp) == 1)) {
        stop(paste("The length of the n_tested vector must be equal to the dp vector.",
                   "ie. you must describe both the number fo animals tested in each",
                   "group as well as the dp in each group."), sep = "\n")
    }

    if (!(length(n_tested) == length(test_Se) | length(test_Se) == 1)) {
        stop("Length of test_Se must be 1 or the length of n_tested")
    }

    A <- 1 - (dp * test_Se)

    df <- as.data.frame(1 - tapply(A ^ n_tested, INDEX = id, FUN = "prod"))

    names(df) <- c("HSe")

    df$id <- rownames(df)

    df[,c("id", "HSe")]
}

##' Herd Sensitivity
##'
##' Calculate the Herd sensitivity when multiple samples from
##' individual units within the herd. The function uses the assumption
##' of finite population when greater than 10% of units are tested and
##' otherwise the assumption of infinite population.
##'
##' @title hse
##' @param id The herdid
##' @param n_tested The number tested in each URG
##' @param N The number of units in each of the URG
##' @param test_Se The sensitivity of the test (length = 1). If you
##'     have reason to believe that the test sensitivity is different
##'     for different URG. Then supply a vector of Sensitivities. This
##'     could conceivably be because of using different tests for
##'     different samples from different URG.
##' @param dp The is a vector (length 1) of the the design prevalance
##'     (df) in the case where there is only one unit risk group (URG)
##'     in the herd. Or a vector (length n) of EPIn for each of the
##'     URG in the herd.
##' @param threshold The breakpoint above which the finite population
##'     size calculation will be used. The default is 0.1 which means
##'     that if > 10% of animals are tested in a herd the finite
##'     popualation will be assumed; less than or equal to 10%, the
##'     infinite population will be assumed.
##' @param force If force = FALSE (default) then the function errors
##'     if n>N. If force = TRUE then this is allowed and uses the
##'     hse_infinite to calculate HSe.
##' @return A vector (length 1)
##' @export
##' @examples
##' df <- data.frame(id = seq(1:20),
##'                  n_tested = rpois(20, 6),
##'                  N = rpois(20, 50),
##'                  test_Se = 0.3,
##'                  dp = 0.05)
##' ## Calculate the herd level sensitivity for each of these herds. If
##' ## the ratio of the number tested to number of animals in the herd
##' ## exceeds the threshold then the finite method is used, otherwise the
##' ## infinite method is used.
##' hse(df$id,
##'     df$n_tested,
##'     df$N,
##'     df$test_Se,
##'     df$dp,
##'     threshold = 0.1)
hse <- function(id,
                n_tested,
                N,
                test_Se,
                dp,
                threshold = 0.1,
                force = FALSE) {

    ## Ratio of animals tested in the herds
    ratio <- n_tested / N

    ## Check if this is more than expected
    if (any(ratio > 1) & !force) {
        problem <- id[ratio > 1]
        stop(paste("Greater than 100% of animals cannot be tested.",
                   "This occurs in the following ids:",
                   paste(problem, collapse = ", "),
                   "To ignore this an default to infinite population",
                   "for these herds, set force = TRUE", sep = "\n"))
    }

    ## Use the finite calculation for those with more than the threshhold
    finite <- NULL
    index_finite <- (ratio > threshold) & (ratio < 1)
    test_Se_finite <- test_Se
    if (length(test_Se) > 1) {
        test_Se_finite <- test_Se[index_finite]
    }
    dp_finite <- dp
    if (length(dp) > 1) {
        dp_finite <- dp[index_finite]
    }
    if (any(index_finite)) {
        finite <- hse_finite(id[index_finite],
                             n_tested[index_finite],
                             N[index_finite],
                             test_Se_finite,
                             dp_finite)
        finite$method <- "finite"
    }
    if(all(index_finite)) {
        return(finite)
    }

    ## Otherwise use the infinite
    index_infinite <- !index_finite
    test_Se_infinite <- test_Se
    if (length(test_Se) > 1) {
        test_Se_infinite <- test_Se[index_infinite]
    }
    dp_infinite <- dp
    if (length(dp) > 1) {
        dp_infinite <- dp[index_infinite]
    }

    infinite <- hse_infinite(id[index_infinite],
                             n_tested[index_infinite],
                             test_Se_infinite,
                             dp_infinite)
    infinite$method <- "infinite"

    ## return the complete dataset
    rbind(finite,
          infinite)
}

##' Calculate the surveillance system sensitivity
##'
##' Takes a vector of the sensitivity of herds tested in the
##' surveillance system and a vector of the effective probability of
##' infection in the herds (EPIH) to calculate the total surveillance system
##' sensitvity for the entire program.
##' @title sysse
##' @param dp The vector of EPIH for all herds tested in the surveillance system
##' @param hse The calculated hse for all the herds tested in the surveillance system
##' @return A vector (length 1)
##' @export
##' @examples
##' df <- data.frame(id = seq(1:20),
##'                  n_tested = rpois(20, 6),
##'                  N = rpois(20, 50),
##'                  test_Se = 0.3,
##'                  dp = 0.05)
##' ## Calculate the herd level sensitivity for each of these herds. If
##' ## the ratio of the number tested to number of animals in the herd
##' ## exceeds the threshold then the finite method is used, otherwise the
##' ## infinite method is used.
##' herd_Se <- hse(df$id,
##'                df$n_tested,
##'                df$N,
##'                df$test_Se,
##'                df$dp,
##'                threshold = 0.1)
##' ## Calculate the system sensitivity given the testing and sensitivity
##' ## in these herds:
##' sysse(dp = rep(0.10, nrow(herd_Se)),
##'       hse = herd_Se$HSe)
sysse <- function(dp, hse) {

    if (length(hse) != length(dp)) {
        stop(paste("The herd Se vector (hse) must be the same length",
                   "as the Effective probability of infection of the herd (dp)",
                   sep = "\n"))
    }
    if (any(hse > 1 | hse < 0)) {
        stop("At least one of the hse values is greater than 1 or less than 0")
    }
    if (any(dp > 1 | dp < 0)) {
        stop(paste(c("At least one effecitive probability of infection (dp)",
                     "is greater than 1 or less than 0"), sep = "\n"))
    }
    1 - prod(1 - dp * hse)
}

##' Calculate the surveillance system sensitivity for a finite
##' population of herds
##'
##' Takes a vector of the sensitivity of herds tested in the
##' surveillance system and a vector of the effective probability of
##' infection in the herds (EPIH) to calculate the total surveillance
##' system sensitivity for the entire program. This is adjusted for
##' the the total number of herds in the population.
##' @title sysse
##' @param dp The vector of EPIH for all herds tested in the
##'     surveillance system.
##' @param hse The calculated hse for all the herds tested in the
##'     surveillance system.
##' @param N The total number of herds in the population.
##' @return A vector (length 1)
##' @export
##' @examples
##' df <- data.frame(id = seq(1:20),
##'                   n_tested = rpois(20, 6),
##'                   N = rpois(20, 50),
##'                   test_Se = 0.3,
##'                   dp = 0.05)
##'  ## Calculate the herd level sensitivity for each of these herds. If
##'  ## the ratio of the number tested to number of animals in the herd
##'  ## exceeds the threshold then the finite method is used, otherwise the
##'  ## infinite method is used.
##'  herd_Se <- hse(df$id,
##'                 df$n_tested,
##'                 df$N,
##'                 df$test_Se,
##'                 df$dp,
##'                 threshold = 0.1)
##'  ## Calculate the system sensitivity given the testing and sensitivity
##'  ## in these herds adjusted for the total number of herds in the population:
##'  sysse_finite(dp = rep(0.10, nrow(herd_Se)),
##'               hse = herd_Se$HSe,
##'               N = 100)
sysse_finite <- function(dp, hse, N) {

    if (length(hse) != length(dp)) {
        stop(paste("The herd Se vector (hse) must be the same length",
             "as the Effective probability of infection of the herd (dp)",
             sep = "\n"))
    }
    if (any(hse > 1 | hse < 0)) {
        stop("At least one of the hse values is greater than 1 or less than 0")
    }
    if (any(dp > 1 | dp < 0)) {
        stop(paste(c("At least one effecitive probability of infection (dp)",
                     "is greater than 1 or less than 0"), sep = "\n"))
    }
    1 - prod((1 - hse / N) ^ (dp * N))
}
