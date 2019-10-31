# upload_model_plot ------------------------------------------------------------
upload_model_plot <- function(
  user_id, spot_id, model_id, plot_file, title = "title?",
  description = "description?"
)
{
  base_path <- path_models(user_id, spot_id, model_id)

  path <- paste0(base_path, "/upload/plot")

  body <- list(
    upload = httr::upload_file(plot_file),
    description = description,
    title = title
  )

  # Upload the model file using the "upload" endpoint
  result <- safe_postgres_post(path, body = body, encode = "multipart")

  message(result$message)
}

# show_model_plots -------------------------------------------------------------
show_model_plots <- function(user_id, spot_id, model_id)
{
  #kwb.utils::assignPackageObjects("fhpredict")
  #user_id=3;spot_id=67;model_id=80
  model_plot_info <- get_model_plot_info(user_id, spot_id, model_id)

  body <- if (nrow(model_plot_info) == 0) {

    "<h2>No plots available.</h2>"

  } else {

    sprintf(
      "<h2>%s</h2><img src=\"%s\" height=\"300px\" width=\"400px\"><h4>%s</h4>",
      model_plot_info$title, model_plot_info$url, model_plot_info$description
    )
  }

  html_content <- paste(collapse = "<br>\n", c(
    "<html><head><meta charset=\"UTF-8\"></head><body>",
    body,
    "</body></html>"
  ))

  html_file <- tempfile(fileext = ".html")

  writeLines(html_content, con = file(html_file, encoding = "UTF-8"))

  utils::browseURL(html_file)
}

# get_model_plot_info ----------------------------------------------------------
get_model_plot_info <- function(user_id, spot_id, model_id)
{
  model_info <- api_get_model(user_id, spot_id)

  selected <- model_info$id == model_id & ! is.na(model_info$plotfiles.url)

  if (! (any(selected))) {

    return(data.frame())
  }

  model_info <- model_info[selected, ]

  extensions <- c("id", "url", "title", "description")

  plot_columns <- paste0("plotfiles.", extensions)

  stats::setNames(model_info[plot_columns], extensions)
}
