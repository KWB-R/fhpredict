% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_get_timeseries.R
\name{api_get_timeseries}
\alias{api_get_timeseries}
\title{Get Timeseries Data from the Postgres Database Via API}
\usage{
api_get_timeseries(
  path,
  subject = "timeseries",
  sort = TRUE,
  token = NULL,
  type = NULL
)
}
\arguments{
\item{path}{(relative) path to API endpoint}

\item{subject}{name of data subject, to be used in messages}

\item{sort}{if \code{TRUE} (the default), the returned data frame will be
sorted by the "dateTime" column}

\item{token}{passed to \code{fhpredict:::postgres_request}}

\item{type}{optional. If given, columns are removed and reordered using
\code{fhpredict:::remove_and_reorder_columns()}}
}
\description{
This function calls \code{\link{postgres_get}} to get data from the endpoint
given by \code{path} and calls \code{fhpredict:::flatten_recursive_list} to
convert the list structure into a data frame. The time columns "createdAt"
and "updatedAt" are converted from a text timestamp given in ISO 8601 format
(e.g. "2019-10-22T23:38:29.003Z") to POSIXct objects in time zone
"Europe/Berlin". A new POSIXct column \code{dateTime} is created from the
original text columns \code{date} and \code{dateTime} as they are returned by
the API:
\itemize{
  \item{\code{(original) date} column: date and time information as text,
  e.g. "2019-09-21T00:00:00.000Z"}
  \item{\code{(original) dateTime} column: only time information as text,
  e.g. "10:50:00"}
  \item{\code{(new) dateTime} column: date and time as POSIXct in time zone
  "Europe/Berlin"}
}
}
\keyword{internal}
