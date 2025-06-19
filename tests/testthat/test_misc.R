library(testthat)
library(tempdisagg)

context("tsbox support")
library(tsbox)

test_that("to daily works", {
  m <- td(ts_df(mdeaths) ~ 1, to = "day", method = "fast")
  expect_is(predict(m), "data.frame")
})


test_that("tsbox mode has same results as ts", {
  data(swisspharma)
  y <- sales.a
  x <- exports.q
  m0 <- td(y ~ x)

  y <- ts_df(sales.a)
  x <- ts_df(exports.q)
  m1 <- td(y ~ x)

  expect_equal(ts_ts(predict(m1)), predict(m0))
  expect_equal(ts_ts(resid(m1)), resid(m0))
  expect_equal(
    capture_output(summary(m1), print = TRUE),
    capture_output(summary(m0), print = TRUE)
  )
})


test_that("internal NA drops an error", {
  y <- c(2, 2, NA, 2, 2, 2, 2, 2)
  y.ts <- ts(y, start = 2000)

  expect_error(td(y.ts ~ 1, to = "monthly"))

  library(tsbox)
  y.df <- ts_df(y.ts)
  expect_error(td(y.df ~ 1, to = "monthly"))
})
