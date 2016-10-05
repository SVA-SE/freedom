##' Calculate the posterior probability of freedom from the prior and
##' the sensitivity of the system
##'
##' The prior probabailty of freedom at the beginning of the
##' surveillance initiative is a value that is based on some external
##' evidence. Often 0.5 is used as a conservative estimate of the
##' probabaility that the population is free from the disease. For
##' subsequent time intervals in the surveillance system, the prior
##' year's posterior probability of freedom is used (plus a risk of
##' introduction) as the prior probability in this calculation.
##' 
##' @title post_fr 
##' @param prior_fr The prior probability of freedom  
##' @param Se The sensitivity of the surveillance system
##' @return A vector (length 1)
##' @export
##' @author Thomas Rosendal
post_fr <- function(prior_fr, Se){
    if(prior_fr > 1) {
        stop("The prior probability of freedom cannot be greater than 1")
    }
    if(Se > 1) {
        stop("System sensitivity cannot be greater than 1")
    }
    prior_fr / (1 - ((1 - prior_fr) * Se))
}
##' Calculate the prior probability of freedom (year = k)
##'
##' In order to calculate the posterior probability of freedom (year =
##' k) , the prior probability of freedom (year = k) is first
##' calculated from the posterior probability of freedom (year = k-1)
##' from the previous year and the annual probability that the disease
##' is introduced into the population.
##' 
##' @title prior_fr
##' @param post_fr The posterior probability of freedom (year = k-1)
##' @param intro The annual probability of introduction
##' @return A vector (length = 1) The prior probability of freedom (year = k)
##' @export
##' @author Thomas Rosendal
prior_fr <- function(post_fr, intro){
    if(post_fr > 1) {
        stop("The posterior probability of freedom cannot be greater than 1")
    }
    if(intro > 1) {
        stop("The annual probability of introduction cannot be greater than 1")
    }
    post_fr - (post_fr * intro)
}
