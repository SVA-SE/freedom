##' Adjusted Risk
##'
##' Calculate the adjusted risk for each of the unit risk groups
##' (URG). This can be used at both the herd and the animal level. The
##' proportion vector, for herd level, is therefore the proportion
##' herds in the population that are in each of the unit risk
##' groups. The proportion vector for animal level is the proportion
##' of animals within a given herd that are in each URG.
##'
##' @title adjusted_risk
##' @param prop A vector of proportions of the population that belong
##'     to each URG (Unit risk group)
##' @param RR A vector of the relative risks of for each URG. The
##'     first of these is the referent group and therefore must be
##'     equal to 1
##' @return A vector of Adjusted risks
##' @export
##' @examples
##' df <- sample_data(nherds = 100,
##'                  mean_herd_size = 300,
##'                  n_herd_urg = 2,
##'                  herd_dist = c(0.9, 0.1),
##'                  herd_samp_frac = 0.01,
##'                  herd_samp_dist = c(0.3, 0.7),
##'                  n_animal_urg = 1,
##'                  animal_dist = c(1),
##'                  animal_samp_frac = 0.05,
##'                  animal_samp_dist = c(1),
##'                  seed = 1)
##' ## The proportion of herds in each unit risk group
##' table(df$herd_urg)/nrow(df)
##' ## Calculate the Adjusted risk for each unit risk group based on the
##' ## proportion in each group and the estimated relative risk of being
##' ## in that group:
##' AR <- freedom::adjusted_risk(as.numeric(table(df$herd_urg)/nrow(df)),
##'                              c(1, 2.3))
adjusted_risk <- function(prop, RR) {

    if (length(prop) != length(RR)) {
        stop("The length of the proportions vector must be equal to the length of the RR vector")
    }

    if (RR[1] != 1) {
        stop("The relative risk of the first URG must be 1. This is the referent group")
    }

    if (round(sum(prop), 10) != 1) {
        stop("The proportion vector must sum to 1")
    }

    unlist(
        lapply(seq_len(length(prop)), function(x){
            RR[x] / sum(RR * prop)
        })
    )
}

##' EffProbInf
##'
##' Calculate the effective probability of infection (EPI) for each unsit
##' risk group in the population. This could be either at the herd
##' level or within herd level. The dp for herds is therefore the
##' minimum prevlance among herds that you would like to design the
##' surveillance system to be able to detect. The dp for within herds
##' is therefore the minimum prevalance of the disease within a herd
##' among the animals that you would like to design the surveillance
##' system to detect.
##'
##' @title EffProbInf
##' @param dp A vector The design prevalence
##' @param AR A vector of the adjusted risks of the unit risk groups
##' @return A vector of EPI
##' @export
##' @examples
##' df <- sample_data(nherds = 100,
##'                  mean_herd_size = 300,
##'                  n_herd_urg = 2,
##'                  herd_dist = c(0.9, 0.1),
##'                  herd_samp_frac = 0.01,
##'                  herd_samp_dist = c(0.3, 0.7),
##'                  n_animal_urg = 1,
##'                  animal_dist = c(1),
##'                  animal_samp_frac = 0.05,
##'                  animal_samp_dist = c(1),
##'                  seed = 1)
##' ## The proportion of herds in each unit risk group
##' table(df$herd_urg)/nrow(df)
##' ## Calculate the Adjusted risk for each unit risk group based on the
##' ## proportion in each group and the estimated relative risk of being
##' ## in that group:
##' AR <- freedom::adjusted_risk(as.numeric(table(df$herd_urg)/nrow(df)),
##'                              c(1, 2.3))
##' EPHI <- EffProbInf(0.05, AR)
EffProbInf <- function(dp, AR) {

    if (!(length(dp) == 1 | length(dp) == length(AR))) {
        stop("The design prevalence (dp) vector must be length 1 or be equal in length to the AR vector")
    }

    epi <- dp * AR

    if(any(epi >=1)){
        warning(paste(c("The EPI should not be greater than 1 for any URG.",
                        "Consider your choices of design prevalance and the",
                        "relative risks of the URG"), sep = "\n"))
    }
    epi
}
