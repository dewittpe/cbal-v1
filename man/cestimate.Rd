% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{cestimate}
\alias{cestimate}
\title{Function for Estimating Causal Effects with \code{cbal}.}
\usage{
cestimate(obj, Y, method = c("sandwich", "bootstrap"),
  boot_iter = 1000, ...)
}
\arguments{
\item{obj}{object of class "cbalance".}

\item{Y}{numeric outcome vector.}

\item{method}{method for estimating the variance of the treatment effect estimate.}

\item{boot_iter}{the number of bootstrap resamples if method = "bootstrap" is selected.}
}
\description{
This function returns the Horvitz-Thompson estimates and the sandwich variance
estimate from an object of type \code{cbalance}.
}
