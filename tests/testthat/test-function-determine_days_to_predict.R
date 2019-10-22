test_that("determine_days_to_predict() works", {

  f <- fhpredict:::determine_days_to_predict

  date <- "2019-07-05"

  expect_identical(
    f(user_id = -1), seq(as.Date("2019-09-26"), by = 1, length.out = 5)
  )

  expect_identical(f(from = date), as.Date(date))
  expect_identical(f(to = date), as.Date(date))
  expect_identical(f(from = date, to = date), as.Date(date))
})
