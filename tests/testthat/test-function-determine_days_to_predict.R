test_that("determine_date_range() works", {

  f <- fhpredict:::determine_date_range

  date <- "2019-07-05"

  expect_identical(f(from = date), as.Date(date))
  expect_identical(f(to = date), as.Date(date))
  expect_identical(f(from = date, to = date), as.Date(date))
})
