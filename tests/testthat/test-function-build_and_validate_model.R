test_that("build_and_validate_model() throws errors", {

  spot_data_0 <- list(my_river = list())
  spot_data_1 <- list(a = data.frame(), b = data.frame())
  spot_data_2 <- list(a_1 = data.frame(), b_2 = data.frame())
  spot_data_3 <- list(hygiene_1 = data.frame(), b_2 = data.frame())

  f <- fhpredict:::build_and_validate_model

  expect_error(f(spot_data_0, prefix = "my_river"), "list of data frames")
  expect_error(f(spot_data_1, prefix = "my_river"), "do not have a prefix")

  expect_error(expect_message(regexp = "do not have a prefix",
    f(spot_data_2, prefix = "my_river")
  ))

  expect_error(expect_message(regexp = "unexpected prefixes",
    f(spot_data_3, prefix = "my_river")
  ))
})
