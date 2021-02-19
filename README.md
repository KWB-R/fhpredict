[![R-CMD-check](https://github.com/KWB-R/fhpredict/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/fhpredict/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/fhpredict/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/fhpredict/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/fhpredict/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/fhpredict)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fhpredict)]()
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3386817.svg)](https://doi.org/10.5281/zenodo.3386817)


# fhpredict

This R package provides all functions that are
required for the Web application that is developed within the
[Flusshygiene](https://www.kompetenz-wasser.de/en/project/flusshygiene/) project.

## Installation

```r
#install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("KWB-R/fhpredict")
```
## Setup

The function `get_radolan_urls_bucket()` requires two environment variables to 
be set:

* `FHPREDICT_RADOLAN_API_URL_PROD`: Endpoint to the service that returns URLs to
Radolan files, 
* `FHPREDICT_RADOLAN_API_TOKEN_PROD`: token that is required to access the 
endpoint. 

All functions that make requests to the Postgres API require the following
environment variables to be set:

* `FHPREDICT_PG_API_URL`
* `AUTH0_REQ_URL`
* `AUTH0_CLIENT_ID` 
* `AUTH0_CLIENT_SECRET`
* `AUTH0_AUDIENCE`

The following additional envirionment variables are required to download Radolan
files from the AWS S3 server:

* `AWS_ACCESS_KEY_ID`
* `AWS_SECRET_ACCESS_KEY`
* `AWS_DEFAULT_REGION`

## Main functions

* [`provide_rain_data()`](https://kwb-r.github.io/fhpredict/dev/reference/provide_rain_data.html)
* [`build_model()`](https://kwb-r.github.io/fhpredict/dev/reference/build_model.html)
* [`predict_quality()`](https://kwb-r.github.io/fhpredict/dev/reference/predict_quality.html)

## Documentation

Release: [https://kwb-r.github.io/fhpredict](https://kwb-r.github.io/fhpredict)

Development: [https://kwb-r.github.io/fhpredict/dev](https://kwb-r.github.io/fhpredict/dev)
