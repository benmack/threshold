% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/threshold_nmm.R
\name{threshold_nmm}
\alias{threshold_nmm}
\title{Thresholding based on normal mixture modelling.}
\usage{
threshold_nmm(x, x_eval = 101, x_pos = NULL, ...)
}
\arguments{
\item{x}{vector of (pixel) gray levels to be thresholded, or an object of class densityMclust Mclust}

\item{x_eval}{...}

\item{x_pos}{A sample of the positive class. If given the overlap between the theoretical positive distribution and the empirical distribution is calculated.}

\item{...}{arguemnts passed to mclust}
}
\description{
It is assumed that the data can be descrived by two Gaussians. 
The parameters of the mixture components are estimated with \code{\link[mclust]{Mclust}}.
}
\references{
...
}

