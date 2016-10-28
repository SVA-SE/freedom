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
##' @author Thomas Rosendal
##' @export
adjusted_risk <- function(prop, RR) {
    if(length(prop) != length(RR)) {
        stop("The length of the proportions vector must be equal to the length of the RR vector")
    }
    if(RR[1] != 1) {
        stop("The relative risk of the first URG must be 1. This is the referent group")
    }
    if(round(sum(prop), 10) != 1) {
        stop("The proportion vector must sum to 1")
    }
    AR <- unlist(
        lapply(seq_len(length(prop)), function(x){
            RR[x]/sum(RR*prop)
        })
    )
    return(AR)
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
##' @author Thomas Rosendal
EffProbInf <- function(dp, AR) {
    if(length(dp) != 1) {
        stop("The design prevalence (dp) vector must be length 1")
    }
    epi <- dp * AR
    if(any(epi >=1)){
        warning("The EPI should not be greater than 1 for any URG.\nConsider your choices of design prevalance and the relative risks of the URG")
    }
    return(epi)
}
