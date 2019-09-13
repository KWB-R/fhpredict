test_that("build_and_validate_model() throws errors", {

  river_data <- list(my_river = list())
  river <- "my_river"

  f <- fhpredict:::build_and_validate_model

  expect_error(f(river_data, river), "not supported")
  expect_error(f(spot_data = river_data, river = river), "not supported")
  expect_error(f(spot_data = river_data, prefix = river), "list of data frames")

})
