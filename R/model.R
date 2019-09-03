# model_to_text ----------------------------------------------------------------
model_to_text <- function(model)
{
  kwb.utils::catAndRun("Converting model object to text", {

    # Create path to a temporary file
    file <- tempfile(fileext = ".txt")

    # Save model to text file
    save(model, file = file, ascii = TRUE)

    # Reread the model as text lines and combine all lines with tab
    paste(readLines(file), collapse = "\t")
  })
}

# text_to_model ----------------------------------------------------------------
text_to_model <- function(text)
{
  stopifnot(is.character(text), length(text) == 1)

  text_lines <- strsplit(text, "\t")[[1]]

  if (text_lines[[1]] != "RDA2") {

    warning(
      "This does not look like a model:\n>>>",
      paste(utils::head(text_lines), collapse = "\n"),
      "<<<\nReturning the original text.", call. = FALSE
    )

    return(text)
  }

  kwb.utils::catAndRun("Converting text to model object", {

    # Create path to a temporary file
    file_path <- tempfile(fileext = ".txt")

    # Open file in binary mode and close file on exit
    con <- file(file_path, "wb")
    on.exit(close(con))

    # Write the text representation of the model to the file using line feed LF
    # which seems to be the line ending used by save(ascii = TRUE)
    write(paste(text_lines, collapse = "\n"), con)

    # Load the model into an R object
    kwb.utils::loadObject(file_path, "model", dbg = FALSE)
  })
}
