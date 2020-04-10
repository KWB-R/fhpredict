test_that("determine_days_to_predict() works", {

  f <- fhpredict:::determine_days_to_predict

  date <- "2019-07-05"

  expect_identical(f(from = date), as.Date(date))
  expect_identical(f(to = date), as.Date(date))
  expect_identical(f(from = date, to = date), as.Date(date))
})
