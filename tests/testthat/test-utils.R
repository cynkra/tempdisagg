
library(testthat)
library(tempdisagg)


# ModeOfSeries Function
# ----------------------------------------------------------------------------

test_that("ModeOfSeries correctly identifies ts objects", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))
  expect_equal(ModeOfSeries(x), "ts")
})

test_that("ModeOfSeries correctly identifies numeric vectors", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(ModeOfSeries(x), "numeric")
})

test_that("ModeOfSeries correctly identifies data.frame objects", {
  x <- data.frame(time = 1:10, value = rnorm(10))
  expect_equal(ModeOfSeries(x), "tsbox")
})

test_that("ModeOfSeries prioritizes xts over ts", {
  skip_if_not_installed("xts")

  # Create xts object (which also inherits from zoo)
  x <- xts::xts(1:10, order.by = seq(as.Date("2000-01-01"), by = "month", length.out = 10))

  # Should return "tsbox" not "ts" even if it has ts-like properties
  expect_equal(ModeOfSeries(x), "tsbox")
})

test_that("ModeOfSeries errors on invalid input types", {
  # Character vector should error
  expect_error(
    ModeOfSeries("not a series"),
    "series must be a time series object or numeric"
  )

  # List should error
  expect_error(
    ModeOfSeries(list(a = 1, b = 2)),
    "series must be a time series object or numeric"
  )

  # NULL should error
  expect_error(
    ModeOfSeries(NULL),
    "series must be a time series object or numeric"
  )
})
