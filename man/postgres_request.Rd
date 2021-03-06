% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/postgres.R
\name{postgres_request}
\alias{postgres_request}
\title{Do a Request to the Flusshygiene Postgres API}
\usage{
postgres_request(
  path,
  type = "GET",
  ...,
  token = NULL,
  config = NULL,
  verbose = FALSE
)
}
\arguments{
\item{path}{relative path to endpoint}

\item{type}{one of "GET", "POST", "DELETE"}

\item{\dots}{further arguments passed to one of \code{\link[httr]{GET}},
\code{\link[httr]{DELETE}}, \code{\link[httr]{POST}}, depending on
\code{type}}

\item{token}{token required to authenticate for the API. If omitted, a token
will be provided by a call to \code{fhpredict:::get_postgres_api_token}.}

\item{config}{optional. Configuration created with
\code{\link[httr]{add_headers}}}

\item{verbose}{if \code{TRUE} (the default is \code{FALSE}), the result of
the request will be printed}
}
\description{
Do a Request to the Flusshygiene Postgres API
}
\keyword{internal}
