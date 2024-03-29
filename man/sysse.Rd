% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/herd_se.R
\name{sysse}
\alias{sysse}
\title{sysse}
\usage{
sysse(dp, hse)
}
\arguments{
\item{dp}{The vector of EPIH for all herds tested in the
surveillance system}

\item{hse}{The calculated hse for all the herds tested in the
surveillance system}
}
\value{
A vector (length 1)
}
\description{
Calculate the surveillance system sensitivity
}
\details{
Takes a vector of the sensitivity of herds tested in the
surveillance system and a vector of the effective probability of
infection in the herds (EPIH) to calculate the total surveillance
system sensitivity for the entire program.
}
\examples{
df <- data.frame(id = seq(1:20),
                 n_tested = rpois(20, 6),
                 N = rpois(20, 50),
                 test_Se = 0.3,
                 dp = 0.05)
## Calculate the herd level sensitivity for each of these herds. If
## the ratio of the number tested to number of animals in the herd
## exceeds the threshold then the finite method is used, otherwise the
## infinite method is used.
herd_Se <- hse(df$id,
               df$n_tested,
               df$N,
               df$test_Se,
               df$dp,
               threshold = 0.1)
## Calculate the system sensitivity given the testing and sensitivity
## in these herds:
sysse(dp = rep(0.10, nrow(herd_Se)),
      hse = herd_Se$HSe)
}
