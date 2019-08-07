skip_on_appveyor()
skip_on_travis()

test_that("api_add_model() works", {

  expect_error(
    fhpredict:::api_add_model()
    # argument "user_id" is missing, with no default
  )

})
