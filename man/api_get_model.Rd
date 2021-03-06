% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_model.R
\name{api_get_model}
\alias{api_get_model}
\title{Read a Model from the Database}
\usage{
api_get_model(user_id, spot_id, model_id = -1L)
}
\arguments{
\item{user_id}{user ID}

\item{spot_id}{bathing spot ID}

\item{model_id}{model ID. A value of -1 will return all available models in a
list}
}
\value{
a model object (if \code{model_id} is given and exists in the
  database) or a list of model objects if \code{model_id = -1}.
}
\description{
This function reads the text representation of a model from the database
and converts it to a model object. If no model_id is given, all available
models for the given user and bathing spot are returned in a list. The
metadata about the models (ids, creation dates, comments) are then returned
in the attribute "model_info".
}
