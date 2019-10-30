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
  model_plot_info <- get_model_plot_info(user_id, spot_id, model_id)

  if (nrow(model_plot_info) == 0) {

    graphics::plot(gridExtra::arrangeGrob(
      grobs = list(grid::textGrob("No plots available."))
    ))

    return()
  }

  pictures <- lapply(model_plot_info$url, function(url) {

    destfile <- file.path(tempdir(), basename(url))

    download.file(url, destfile)

    grImport2::readPicture(destfile)
  })

  titles <- lapply(model_plot_info$title, grid::textGrob)

  assign("prefix", "", envir = grImport2:::.grImport2Env)

  grobs <- lapply(pictures, grImport2::grobify)

  descriptions <- lapply(model_plot_info$description, grid::textGrob)

  graphics::plot(gridExtra::arrangeGrob(
    grobs = c(titles, grobs, descriptions), nrow = 3, heights = c(1, 3, 3)
  ))
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
