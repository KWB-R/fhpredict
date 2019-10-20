# safe_postgres_delete ---------------------------------------------------------
safe_postgres_delete <- function(...)
{
  safe_postgres(postgres_delete, ...)
}

# safe_postgres_get ------------------------------------------------------------
safe_postgres_get <- function(...)
{
  safe_postgres(postgres_get, ...)
}

# safe_postgres_post -----------------------------------------------------------
safe_postgres_post <- function(...)
{
  safe_postgres(postgres_post, ...)
}

# safe_postgres ----------------------------------------------------------------
safe_postgres <- function(fun, ...)
{
  result <- fun(...)
  stop_on_request_failure(result)
  result
}

# stop_on_request_failure ------------------------------------------------------
stop_on_request_failure <- function(
  result, error_text = "", response = attr(result, "response")
)
{
  if (is.null(response)) {
    return()
  }

  status <- httr::http_status(response)

  if (status$category == "Success") {
    return()
  }

  text <- sprintf(
    "HTTP request failed.\nstatus_message: %s",
    new_line_collapsed(status$message)
  )

  if (! is.null(error <- httr::content(response)$error)) {
    text <- sprintf(
      "%s\nerror_message: %s\nerror_detail: %s",
      text,
      new_line_collapsed(error$message),
      new_line_collapsed(error$detail)
    )
  }

  if (nzchar(error_text)) {
    text <- paste0(error_text, "\n", text)
  }

  clean_stop(text)
}
