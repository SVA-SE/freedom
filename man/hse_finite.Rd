% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/herd_se.R
\name{hse_finite}
\alias{hse_finite}
\title{hse_finite}
\usage{
hse_finite(
  id,
  n_tested,
  N,
  test_Se,
  dp,
  rounding = c("none", "ceiling", "round", "floor")
)
}
\arguments{
\item{id}{The herdid.}

\item{n_tested}{The number tested in each URG}

\item{N}{The number of units in each of the URG}

\item{test_Se}{The sensitivity of the test. This may have length
== 1 if all URG and all herds have the same test_Se. It may
also have length(test_Se) == length(n_tested).}

\item{dp}{The design prevalence (dp) could be length(dp) == 1 if
all URG and herds have the same dp. It could alternatively be
length(dp) == length(n_tested) if different design prevalences
are to be applied to each URG.}

\item{rounding}{How should the proportion of animals be rounded?
Default value is 'none' which does no rounding. Other options
are 'round', 'ceiling', and 'floor'. 'round' rounds the dp * N
to the nearest integer and then selects 1 if the value is
0. 'ceiling' takes the ceiling of dp * N, this is consistent
with the method in the Rsurveillance package. 'floor' takes
the floor of dp * N and makes it 1 if the result is 0.}
}
\value{
A data.frame. A dataframe is returned with 2 columns: "id"
    and HSe
}
\description{
Herd Sensitivity calculated with the assumption of a finite
population
}
\details{
Calculate the Herd sensitivity when multiple samples from
individual units within the herd. The function uses the total
population size to adjust the estimates consistent with a finite
population. This method for calculation of HSe is typically used
when greater than 10% of the population has been sampled
}
\examples{
df <- data.frame(id = seq(1:20),
                 n_tested = rpois(20, 5),
                 N = 100,
                 test_Se = 0.3,
                 dp = 0.05)
## Calculate the herd level sensitivity for each of these herds
hse_finite(df$id,
           df$n_tested,
           df$N,
           df$test_Se,
           df$dp)
}
