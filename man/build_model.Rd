% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_model.R
\name{build_model}
\alias{build_model}
\title{Build Water Quality Model for Bathing Spot}
\usage{
build_model(user_id, spot_id, seed = NULL, delete_rain = FALSE)
}
\arguments{
\item{user_id}{user ID}

\item{spot_id}{bathing spot ID}

\item{seed}{if \code{TRUE} the random number generate is initialised using
this value in a call to \code{\link{set.seed}}}

\item{delete_rain}{if \code{TRUE}, all rain data are deleted after successful
model creation. This was used to avoid long loading times (freezing) in the
frontend but should not be necessary any more! The default is \code{FALSE}.}
}
\value{
list with elements \code{data}, \code{success}, \code{message}
}
\description{
Build Water Quality Model for Bathing Spot
}
