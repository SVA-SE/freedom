##' Calculate the posterior probability of freedom from the prior and
##' the sensitivity of the system
##'
##' The prior probability of freedom at the beginning of the
##' surveillance initiative is a value that is based on some external
##' evidence. Often 0.5 is used as a conservative estimate of the
##' probability that the population is free from the disease. For
##' subsequent time intervals in the surveillance system, the prior
##' year's posterior probability of freedom is used (plus a risk of
##' introduction) as the prior probability in this calculation.
##'
##' @title post_fr
##' @param prior_fr The prior probability of freedom
##' @param Se The sensitivity of the surveillance system
##' @return A vector
##' @export
##' @examples
##' ## Calculate the posterior probability of freedom after applying a
##' #sensitivity to a prior probability of freedom:
##' post_pf <- post_fr(0.5, 0.4)
post_fr <- function(prior_fr, Se){

    if (any(prior_fr > 1 | prior_fr < 0)) {
        stop("The prior probability of freedom cannot be greater than 1 or less than 0")
    }

    if (any(Se > 1 | Se < 0)) {
        stop("System sensitivity cannot be greater than 1 or less than 0")
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
##' @return A vector. The prior probability of freedom (year = k)
##' @export
##' @examples
##' ## Calculate the posterior probability of freedom after applying a
##' ## sensitivity to a prior probability of freedom:
##' post_pf <- post_fr(0.5, 0.4)
##' ## Then discount the probability of introduction (0.05) from the
##' ## posterior probability of freedom to calculate the subsequent
##' ## prior probability of freedom for the next time step:
##' prior_pf <- prior_fr(post_pf, 0.05)
prior_fr <- function(post_fr, intro){
    if (any(post_fr > 1 | post_fr < 0)) {
        stop("The posterior probability of freedom cannot be greater than 1 or less than 0")
    }

    if (any(intro > 1 | intro < 0)) {
        stop("The annual probability of introduction cannot be greater than 1 or less than 0")
    }

    post_fr - (post_fr * intro)
}
