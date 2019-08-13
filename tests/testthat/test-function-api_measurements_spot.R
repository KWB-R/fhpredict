skip_on_appveyor()
skip_on_travis()

test_that("api_measurements_spot() works", {

  expect_error(
    fhpredict:::api_measurements_spot()
    # HTTP request failed: Client error: (404) Not Found
  )

})

