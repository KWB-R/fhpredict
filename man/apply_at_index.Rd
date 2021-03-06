% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{apply_at_index}
\alias{apply_at_index}
\title{Apply a Function to List Elements at Given Index}
\usage{
apply_at_index(x, i, fun, ..., prefix = NULL)
}
\arguments{
\item{x}{a list or a data frame (which is in fact a list)}

\item{i}{vector of indices of elements in \code{x} to which the function
\code{fun} is to be applied}

\item{fun}{function to be applied}

\item{\dots}{further arguments passed to \code{fun}}

\item{prefix}{optional. String used to prefix the names of the elements to
which the function was applied. By default the name of the function
\code{fun} is used as a prefix}
}
\description{
Apply a Function to List Elements at Given Index
}
\examples{
(x <- list(a = 1, b = 2, c = 3))
fhpredict:::apply_at_index(x, i = -1, fun = exp)

(x <- data.frame(a = -(1:2), b = -(2:3), c = c("a", "b")))
fhpredict:::apply_at_index(x, i = -3, fun = abs)

}
