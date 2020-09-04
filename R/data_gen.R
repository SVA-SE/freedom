##' A function used to check if a vector of proportions is valid
##'
##' @title valid_proportions
##' @param x numeric
##' @param tolerance a tolerance value
##' @return logical
valid_proportions <- function(x, tolerance = 1e-7) {
    stopifnot(is.numeric(x))

    if(any(x > 1 | x < 0)) {
        return(FALSE)
    }
    if(any(abs(sum(x) - 1) > tolerance)) {
        return(FALSE)
    }
    TRUE
}

##' A function to generate some synthetic data based on a few
##' parameters.
##'
##' @title sample_data
##' @param nherds The total number of herds
##' @param mean_herd_size The mean herd size in the population
##' @param n_herd_urg The number of different herd risk groups
##' @param herd_dist The fraction of herds in each risk group
##' @param herd_samp_frac The total sampling fraction at the herd
##'     level
##' @param herd_samp_dist The fraction of samples to be collected from
##'     each herd risk group
##' @param n_animal_urg The number of animal level risk groups
##' @param animal_dist The fraction of animals within herds that are
##'     part of each risk group
##' @param animal_samp_frac The total sampling fraction of animals
##'     within herds
##' @param animal_samp_dist The fraction of samples that are collected
##'     from each animal risk group
##' @param seed The seed for the random number generator. Default is a
##'     random seed
##' @return A data.frame
##' @import stats
##' @export
##' @examples
##' ## Generate the default example data. This will generate a
##' ## data.frame with a herd identifier (ppn), a herd level unit risk
##' ## group identifier (herd_urg), a animal level unit risk group
##' ## identifier (animal_urg), the total number of animals in the unit
##' ## risk group (N_animal_urg) and the number of animals tested in the
##' ## unit risk group (n_animals_urg).
##'
##' df <- sample_data()
sample_data <- function(nherds = 500,
                        mean_herd_size = 50,
                        n_herd_urg = 2,
                        herd_dist = c(0.8, 0.2),
                        herd_samp_frac = 0.5,
                        herd_samp_dist = c(0.5, 0.5),
                        n_animal_urg = 2,
                        animal_dist = c(0.5, 0.5),
                        animal_samp_frac = 0.15,
                        animal_samp_dist = c(0.5, 0.5),
                        seed = NULL) {

    if(length(herd_dist) != n_herd_urg) {
        stop("The length of the herd distribution vector must be equal to the number of herd unit risk groups")
    }

    if(length(herd_samp_dist) != n_herd_urg) {
        stop("The length of the herd sample distribution vector must be equal to the number of herd unit risk groups")
    }

    if(length(animal_dist) != n_animal_urg) {
        stop("The length of the animal distribution vector must be equal to the number of animal unit risk groups")
    }

    if(length(animal_samp_dist) != n_animal_urg) {
        stop("The length of the animal sample distribution vector must be equal to the number of animal unit risk groups")
    }

    if(!valid_proportions(herd_dist)) {
        stop("The distribution of herds between the herd unit risk groups must be between 0 and 1 and must sum to 1")
    }

    if(!valid_proportions(herd_samp_dist)) {
        stop("The distribution of herd SAMPLES between the herd unit risk groups must be between 0 and 1 and must sum to 1")
    }


    if(!valid_proportions(animal_dist)){
        stop("The distribution of animals between the animal unit risk groups must be between 0 and 1 and sum to 1")
    }

    if(!valid_proportions(animal_samp_dist)) {
        stop("The distribution of animal SAMPLES between the animal unit risk groups must be between 0 and 1 and sum to 1")
    }

    if(!is.null(seed)) {
        set.seed(seed)
    }

    ppn <- 1:nherds
    N <- rpois(nherds, mean_herd_size)
    n <- floor((rpois(nherds, animal_samp_frac * 100) / 100) * N)
    herd_urg <- sample(x = factor(seq_len(n_herd_urg)),
                       size = nherds,
                       replace = TRUE,
                       prob = herd_dist)
    herds <- data.frame(ppn, N, n, herd_urg,
                        stringsAsFactors = FALSE)
    herds <- do.call("rbind", lapply(seq_len(n_herd_urg), function(x) {
        df <- herds[herds$herd_urg == x,]
        df$sample <- sample(c(0, 1), nrow(df), replace = TRUE, prob = c(1 - herd_samp_dist[x], herd_samp_dist[x]))
        return(df)
    }))

    herds$n <- herds$n * herds$sample

    herds <- do.call("rbind", lapply(ppn, function(x) {

        df <- herds[ppn == x,]

        N_animal_urg <- as.vector(table(sample(x = seq_len(n_animal_urg),
                               size = df$N,
                               replace = TRUE,
                               prob = animal_dist)))

        n_sample_animal_urg <- as.vector(table(factor(sample(x = seq_len(n_animal_urg),
                               size = df$n,
                               replace = TRUE,
                               prob = animal_samp_dist), levels = seq_len(n_animal_urg))))

        index <- n_sample_animal_urg > N_animal_urg
        if (any(index)) {
            n_sample_animal_urg[index] <- N_animal_urg[index]
        }

        for(i in seq_len(n_animal_urg - 1)) {
            df <- rbind(df, herds[ppn == x,])
        }

        df$animal_urg <- seq_len(n_animal_urg)
        df$N_animal_urg <- N_animal_urg
        df$n_animal_urg <- n_sample_animal_urg

        df
    }))

    herds <- subset(herds, select = -c(sample, N, n))
    rownames(herds) <- NULL
    herds
}
