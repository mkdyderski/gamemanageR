% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_and_se.R
\name{mean.ray}
\alias{mean.ray}
\title{mean.ray}
\usage{
\method{mean}{ray}(x, area)
}
\arguments{
\item{x}{a vector containing observations, e.g. game densities}

\item{area}{a vector containing areas of each drives in ha}
}
\value{
\describe{
\item{value}{ estimated value computed from vector with Rayleigh distribution using \code{getRayParam} from \code{shotGroups} package}}
}
\description{
\code{mean.ray}  compute estimated value from vector of Rayleigh distribution
}
\details{
\usage{mean.ray(x, area)}

Function computes expected value and multiply by percent of area where species was observed. In case of n<2, weighted mean is used instead.Here will be described how it works TBA
}

