# postgres_get -----------------------------------------------------------------
postgres_get <- function(path)
{
  url <- paste0(assert_final_slash(get_environment_var("API_URL")), path)
  
  token <- get_postgres_api_token()
  
  config <- httr::add_headers("Authorization" = paste("Bearer", token))
  
  response <- httr::GET(url, config = config)
  
  parsed <- httr::content(response, "parsed")
  
  status <- httr::status_code(response)
  
  if (status != 200) {
    error <- kwb.utils::defaultIfNULL(parsed$message, "")
    message(sprintf("Get request '%s' returned with error:\n'%s'", path, error))
    return(structure(list(), response = response))
  }
  
  parsed
}

