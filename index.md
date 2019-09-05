[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/fhpredict?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/fhpredict/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/fhpredict.svg?branch=master)](https://travis-ci.org/KWB-R/fhpredict)
[![codecov](https://codecov.io/github/KWB-R/fhpredict/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/fhpredict)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fhpredict)]()

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

* `ENDPOINT_PROD`: Endpoint to the service that returns URLs to Radolan files, 
* `TOKEN_PROD`: token that is required to access the endpoint. 

All functions that make requests to the Postgres API require the following
environment variables to be set:

* `API_URL`
* `AUTH0_REQ_URL`
* `AUTH0_CLIENT_ID` 
* `AUTH0_CLIENT_SECRET`
* `AUTH0_AUDIENCE`
