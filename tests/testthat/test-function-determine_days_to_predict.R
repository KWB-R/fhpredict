test_that("determine_days_to_predict() works", {

  f <- fhpredict:::determine_days_to_predict

  date <- "2019-07-05"

  hygiene <- data.frame(
    datum = as.POSIXct(c("2000-01-01 00:00:00", "2000-01-02 00:00:00")),
    `e.coli` = c(1, -1)
  )

  expect_identical(f(), Sys.Date())
  expect_identical(f(from = date), as.Date(date))
  expect_identical(f(to = date), as.Date(date))
  expect_identical(f(from = date, to = date), as.Date(date))
  expect_identical(f(hygiene = hygiene), as.Date("2000-01-02"))
  expect_identical(f(hygiene = hygiene[1, ]), Sys.Date())
})
