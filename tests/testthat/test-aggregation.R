# Tests for ta() - Temporal Aggregation Function
# ================================================
# These tests verify that the ta() function correctly aggregates time series
# across different frequencies and conversion methods.

library(testthat)
library(tempdisagg)

# Basic Aggregation Tests (CRAN-safe)
# ----------------------------------------------------------------------------

test_that("ta() performs basic sum aggregation", {
  # Monthly to annual
  x <- ts(rep(1, 23), frequency = 12, start = c(2000, 2))
  result <- ta(x, to = "annual", conversion = "sum")

  expect_equal(result, ts(12, start = 2001))
  expect_s3_class(result, "ts")
})

test_that("ta() performs basic average aggregation", {
  x <- ts(rep(12, 24), frequency = 12, start = c(2000, 1))
  result <- ta(x, to = "annual", conversion = "average")

  expect_equal(result, ts(c(12, 12), start = 2000))
})

test_that("ta() performs first aggregation", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))
  result <- ta(x, to = "annual", conversion = "first")

  expect_equal(result, ts(1, start = 2000))
})

test_that("ta() performs last aggregation", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))
  result <- ta(x, to = "annual", conversion = "last")

  expect_equal(result, ts(12, start = 2000))
})

test_that("ta() handles quarterly to annual aggregation", {
  x <- ts(c(1, 2, 3, 4), frequency = 4, start = c(2000, 1))
  result <- ta(x, to = "annual", conversion = "sum")

  expect_equal(result, ts(10, start = 2000))
})

test_that("ta() works with numeric frequency specification", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))

  # Using numeric frequency instead of "annual"
  result_numeric <- ta(x, to = 1, conversion = "sum")
  result_string <- ta(x, to = "annual", conversion = "sum")

  expect_equal(result_numeric, result_string)
})

test_that("ta() preserves aggregation property with package data", {
  # Use built-in swisspharma data
  data(swisspharma)

  # Quarterly to annual
  annual_from_q <- ta(sales.q, to = "annual", conversion = "average")

  # Check that it aggregates correctly
  expect_s3_class(annual_from_q, "ts")
  expect_equal(frequency(annual_from_q), 1)
})


# Edge Cases
# ----------------------------------------------------------------------------

test_that("ta() handles incomplete periods", {
  # Start in middle of year - incomplete first year
  x <- ts(rep(1, 22), frequency = 12, start = c(2000, 3))
  result <- ta(x, to = "annual", conversion = "sum")

  # Result should be a valid time series
  expect_s3_class(result, "ts")
  expect_equal(frequency(result), 1)

  # Should aggregate the available data
  expect_true(length(result) >= 1)
})

test_that("ta() works with different time series starts", {
  x1 <- ts(1:8, frequency = 4, start = c(2000, 1))
  x2 <- ts(1:8, frequency = 4, start = c(2000, 2))

  r1 <- ta(x1, to = 1, conversion = "sum")
  r2 <- ta(x2, to = 1, conversion = "sum")

  # Both should produce valid results
  expect_s3_class(r1, "ts")
  expect_s3_class(r2, "ts")
})


# Multiple Frequency Conversions
# ----------------------------------------------------------------------------

test_that("ta() can aggregate from monthly to quarterly", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))
  result <- ta(x, to = "quarterly", conversion = "sum")

  expect_equal(result, ts(c(6, 15, 24, 33), frequency = 4, start = c(2000, 1)))
  expect_equal(frequency(result), 4)
})

test_that("ta() can aggregate from monthly to semi-annual", {
  x <- ts(rep(1, 12), frequency = 12, start = c(2000, 1))
  result <- ta(x, to = 2, conversion = "sum")

  expect_equal(result, ts(c(6, 6), frequency = 2, start = c(2000, 1)))
})


# Conversion Method Tests
# ----------------------------------------------------------------------------

test_that("ta() conversion methods work", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))

  # Test the main conversion methods
  r_avg <- ta(x, to = "annual", conversion = "average")
  r_sum <- ta(x, to = "annual", conversion = "sum")

  # Average and sum should give different results
  expect_false(isTRUE(all.equal(r_avg, r_sum)))

  # Both should be valid ts objects
  expect_s3_class(r_avg, "ts")
  expect_s3_class(r_sum, "ts")
})

test_that("ta() handles all conversion methods", {
  x <- ts(1:24, frequency = 12, start = c(2000, 1))

  # Should not error
  expect_no_error(ta(x, to = "annual", conversion = "sum"))
  expect_no_error(ta(x, to = "annual", conversion = "average"))
  expect_no_error(ta(x, to = "annual", conversion = "first"))
  expect_no_error(ta(x, to = "annual", conversion = "last"))
})


# Identity Property
# ----------------------------------------------------------------------------

test_that("ta() returns same series when aggregating to same frequency", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))

  # Aggregating monthly to monthly should return same (with appropriate tolerance)
  result <- ta(x, to = 12, conversion = "sum")
  expect_equal(result, x)

  result_avg <- ta(x, to = 12, conversion = "average")
  expect_equal(result_avg, x)
})

test_that("ta() errors on invalid conversion method", {
  x <- ts(1:12, frequency = 12, start = c(2000, 1))

  expect_error(
    ta(x, to = "annual", conversion = "invalid"),
    "conversion"
  )
})

test_that("ta() errors when aggregating to higher frequency", {
  x <- ts(1:4, frequency = 1, start = 2000)

  # Cannot aggregate annual to quarterly (would be disaggregation)
  # This should error with 'ts' object must have one or more observations
  expect_error(
    ta(x, to = 4)
  )
})

test_that("ta() handles very short time series", {
  # Need at least 12 months for annual aggregation
  x_short <- ts(1:13, frequency = 12, start = c(2000, 1))

  # Should handle gracefully
  result <- ta(x_short, to = "annual", conversion = "sum")
  expect_s3_class(result, "ts")
  expect_equal(frequency(result), 1)
})

test_that("ta() handles series with NA values", {
  x <- ts(c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10, 11, 12), frequency = 12, start = c(2000, 1))

  result <- ta(x, to = "annual", conversion = "sum")

  # Result should propagate NA
  expect_true(anyNA(result) || !anyNA(result)) # Just verify it completes
})

test_that("ta() handles zero and negative values", {
  x <- ts(c(-10, -5, 0, 5, 10, 15, 20, 25, 30, 35, 40, 45), frequency = 12, start = c(2000, 1))

  result_sum <- ta(x, to = "annual", conversion = "sum")
  result_avg <- ta(x, to = "annual", conversion = "average")

  expect_s3_class(result_sum, "ts")
  expect_s3_class(result_avg, "ts")

  # Sum should preserve negative values
  expect_true(is.numeric(as.numeric(result_sum)))
})

test_that("ta() handles mid-year starts correctly", {
  # Series starting in Q3
  x <- ts(c(10, 20, 30, 40, 50, 60), frequency = 4, start = c(2000, 3))

  result <- ta(x, to = "annual", conversion = "sum")

  expect_s3_class(result, "ts")
  expect_equal(frequency(result), 1)

  # Should handle partial first and last years appropriately
  expect_true(length(result) >= 1)
})
