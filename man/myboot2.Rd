% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{Number of iterations that function will make.}

\item{x}{The data set to sample from.}

\item{fun}{The function that will be applied to the data set.}

\item{alpha}{The alpha value for the confidence interval.}

\item{cx}{character expansion factor.}

\item{...}{Further graphical parameters}
}
\value{
Graph of bootstrap data
}
\description{
Creates a bootstrap method for a data set.
}
