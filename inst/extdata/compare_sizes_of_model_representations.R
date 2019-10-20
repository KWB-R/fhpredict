# Model object in R
models <- list(
  kwb.flusshygiene.app:::model_grunewaldturm,
  kwb.flusshygiene.app:::model_kleine_badewiese
)

sizes <- lapply(models, function(model) {
  # Save model to binary file (.RData)
  file_rdata <- tempfile(fileext = ".RData")
  save(model, file = file_rdata)

  list(
    size_object = object.size(model),
    size_rdata = file.info(file_rdata)$size,
    size_text = object.size(kwb.utils:::objectToText(model))
  )
})

str(sizes)
