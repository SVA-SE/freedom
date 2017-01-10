##' Herd Sensitivity calculated with the assumption of a finite population
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
##' @author Thomas Rosendal
hse_finite <- function(id, n_tested, N, test_Se, dp) {
    if(length(n_tested) != length(N)) {
        stop("The length of the n_tested vector must be equal to the N vector.\nie. you must describe both the number of animals tested in each group as well as how many animals are in each group.")
    }
    if(any(n_tested > N)) {
        stop("One of the URG has more subunits tested than in the population")
    }
    if(!(length(dp) == 1 | length(n_tested) == length(dp))) {
        stop("The length of the n_tested vector must be equal to the dp vector.\nie. you must describe both the number of animals tested in each group as well as the dp in each group.")
    }
    if(!(length(test_Se) == 1 | length(test_Se) == length(n_tested))) {
        stop("The length of test_Se must be either 1 or the length of n_tested")
    }
    A <- 1 - (n_tested * test_Se / N)
    B <- dp * N
    df <- as.data.frame(1 - tapply(A ^ B, INDEX = id, FUN = "prod"))
    names(df) <- c("HSe")
    return(df)
}

##' Herd Sensitivity calculated with the assumption of an infinite population
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
##' @author Thomas Rosendal
hse_infinite <- function(id, n_tested, test_Se, dp){
    if(!(length(n_tested) == length(dp) | length(dp) == 1)) {
        stop("The length of the n_tested vector must be equal to the dp vector.\nie. you must describe both the number fo animals tested in each group as well as the dp in each group.")
    }
    if(!(length(n_tested) == length(test_Se) | length(test_Se) == 1)) {
        stop("Length of test_Se must be 1 or the length of n_tested")
    }
    A <- 1 - (dp * test_Se)
    df <- as.data.frame(1 - tapply(A ^ n_tested, INDEX = id, FUN = "prod"))
    names(df) <- c("HSe")
    df$id <- rownames(df)
    df <- df[,c("id", "HSe")]
    return(df)
}

##' Herd Sensitivity 
##'
##' Calculate the Herd sensitivity when multiple samples from
##' individual units within the herd. The function uses the assumption
##' of finite population when greater than 10% of units are tested and
##' otherwise the assumption of infinite population.
##' 
##' @title hse
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
##' @return A vector (length 1)
##' @export
##' @author Thomas Rosendal
hse <- function(n_tested, N, test_Se, dp){
    if(sum(n_tested)/sum(N)>1) {
        stop("Greater than 100% of animals cannot be tested")
    }
    if(sum(n_tested)/sum(N) > 0.1) {
        result <- hse_finite(n_tested, N, test_Se, dp)
        return(result)
    } else {
        result <- hse_infinite(n_tested, test_Se, dp)
        return(result)
    }
}

##' Calculate the surveillance system sensitivity
##'
##' Takes a vector of the sensitivity of herds tested in the
##' surveillance system and a vector of the effective probability of
##' infection in the herds (EPIH) to calculate the total surveillance system
##' sensitvity for the entire program.
##' @title sysse
##' @param dp The vector of EPIH for all herds tested in the surveillance system
##' @param hse The calculated hse for all the herds tested in teh surveillance system
##' @return A vector (length 1)
##' @export
##' @author Thomas Rosendal
sysse <- function(dp, hse) {
    if(length(hse) != length(dp)) {
        stop("The herd Se vector (hse) must be the same length as the Effective probability of infection of the herd (dp)")
    }
    if(any(hse > 1)) {
        stop("At least one of the hse values is greater than 1")
    }
    if(any(dp > 1)) {
        stop("At least one effecitive probability of infection (dp) is greater than 1")
    }
    1 - prod(1 - dp * hse)
}
