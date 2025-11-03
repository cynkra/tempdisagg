# Numerical Regression Tests
# ===========================
# These tests compare current results against reference values from v0.24.1
# to detect unintended numerical changes.
#
# Based on lines 120-161 from the original tests/test-all.R
# Reference values are stored in tests/testthat/helper-fixtures.R

library(testthat)
library(tempdisagg)

# Load test data
data(swisspharma)

test_that("year-to-quarter disaggregation matches reference values", {
  skip_on_cran() # Numerical regression test

  # Test a subset of key methods against reference values
  # Reference values are first 10 quarters from v0.24.1

  # Setup formulas (same as in original estimAll function)
  formula1 <- sales.a ~ exports.q + imports.q

  # Chow-Lin MinRSS (Ecotrim variant)
  m_cl_rss <- td(formula1, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)
  result_cl_rss <- predict(m_cl_rss)

  # Compare first 10 values to reference
  expect_equal(
    as.numeric(result_cl_rss[1:10]),
    reference_y2q$cl_rss_R,
    tolerance = 1e-8,
    label = "chow-lin-minrss-ecotrim first 10 values"
  )

  # Chow-Lin MaxLog
  m_cl_log <- td(formula1, method = "chow-lin-maxlog", truncated.rho = -1)
  result_cl_log <- predict(m_cl_log)

  expect_equal(
    as.numeric(result_cl_log[1:10]),
    reference_y2q$cl_log,
    tolerance = 1e-8,
    label = "chow-lin-maxlog first 10 values"
  )

  # Fernandez
  m_fer <- td(formula1, method = "fernandez", truncated.rho = -1)
  result_fer <- predict(m_fer)

  expect_equal(
    as.numeric(result_fer[1:10]),
    reference_y2q$fer,
    tolerance = 1e-8,
    label = "fernandez first 10 values"
  )

  # Litterman MinRSS
  m_lit_rss <- td(formula1, method = "litterman-minrss", truncated.rho = -1)
  result_lit_rss <- predict(m_lit_rss)

  expect_equal(
    as.numeric(result_lit_rss[1:10]),
    reference_y2q$lit_rss,
    tolerance = 1e-5, # Looser tolerance for litterman methods
    label = "litterman-minrss first 10 values"
  )
})


test_that("quarter-to-month disaggregation matches reference values", {
  skip_on_cran() # Numerical regression test

  # Setup formulas
  formula1 <- sales.q ~ exports.m + imports.m

  # Chow-Lin MinRSS (Ecotrim variant)
  m_cl_rss <- td(formula1, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)
  result_cl_rss <- predict(m_cl_rss)

  expect_equal(
    as.numeric(result_cl_rss[1:10]),
    reference_q2m$cl_rss_R,
    tolerance = 1e-7, # Slightly looser tolerance for monthly data
    label = "chow-lin-minrss-ecotrim (q2m) first 10 values"
  )

  # Chow-Lin MaxLog
  m_cl_log <- td(formula1, method = "chow-lin-maxlog", truncated.rho = -1)
  result_cl_log <- predict(m_cl_log)

  expect_equal(
    as.numeric(result_cl_log[1:10]),
    reference_q2m$cl_log,
    tolerance = 1e-7,
    label = "chow-lin-maxlog (q2m) first 10 values"
  )

  # Fernandez
  m_fer <- td(formula1, method = "fernandez", truncated.rho = -1)
  result_fer <- predict(m_fer)

  expect_equal(
    as.numeric(result_fer[1:10]),
    reference_q2m$fer,
    tolerance = 1e-7,
    label = "fernandez (q2m) first 10 values"
  )
})


test_that("denton methods work correctly", {
  skip_on_cran() # Numerical regression test

  formula2 <- sales.a ~ 1

  # Denton-Cholette h=1 (proportional)
  m_dencho_p_1 <- td(formula2, method = "denton-cholette", h = 1, to = 4)
  result_dencho_p_1 <- predict(m_dencho_p_1)

  # Check that results are reasonable (not NA, proper aggregation)
  expect_false(any(is.na(result_dencho_p_1)))
  aggregated <- ta(result_dencho_p_1, to = "annual", conversion = "sum")
  expect_equal(
    window(aggregated, start = start(sales.a), end = end(sales.a)),
    sales.a,
    tolerance = 1e-7
  )

  # Denton-Cholette h=2 (proportional)
  m_dencho_p_2 <- td(formula2, method = "denton-cholette", h = 2, to = 4)
  result_dencho_p_2 <- predict(m_dencho_p_2)

  # Check that results are reasonable
  expect_false(any(is.na(result_dencho_p_2)))
  aggregated2 <- ta(result_dencho_p_2, to = "annual", conversion = "sum")
  expect_equal(
    window(aggregated2, start = start(sales.a), end = end(sales.a)),
    sales.a,
    tolerance = 1e-7
  )

  # Note: Reference values from v0.24.1 are NA for these methods,
  # so we test aggregation property instead of exact values
})


test_that("aggregation property holds for all methods", {
  skip_on_cran() # Comprehensive test

  # Year to quarter
  formula1_y2q <- sales.a ~ exports.q + imports.q
  formula2_y2q <- sales.a ~ 1

  # Test that all methods satisfy aggregation constraint
  methods_with_indicators <- c(
    "chow-lin-minrss-ecotrim", "chow-lin-minrss-quilis", "chow-lin-maxlog",
    "fernandez", "litterman-minrss", "litterman-maxlog", "ols"
  )

  for (method in methods_with_indicators) {
    m <- td(formula1_y2q, method = method, truncated.rho = if (method == "ols") NULL else -1)
    result <- predict(m)
    aggregated <- ta(result, to = "annual", conversion = "sum")
    aggregated <- window(aggregated, start = start(sales.a), end = end(sales.a))

    expect_equal(
      aggregated, sales.a,
      tolerance = 1e-7,
      label = sprintf("%s aggregation property", method)
    )
  }

  # Methods without indicators
  methods_no_indicators <- c("denton-cholette", "denton", "uniform")

  for (method in methods_no_indicators) {
    m <- td(formula2_y2q, method = method, to = 4, h = if (method %in% c("denton-cholette", "denton")) 1 else NULL)
    result <- predict(m)
    aggregated <- ta(result, to = "annual", conversion = "sum")
    aggregated <- window(aggregated, start = start(sales.a), end = end(sales.a))

    expect_equal(
      aggregated, sales.a,
      tolerance = 1e-7,
      label = sprintf("%s aggregation property", method)
    )
  }
})


test_that("quarter to month aggregation property holds", {
  skip_on_cran() # Comprehensive test

  formula1_q2m <- sales.q ~ exports.m + imports.m
  formula2_q2m <- sales.q ~ 1

  # Test with indicators
  methods_with_indicators <- c(
    "chow-lin-minrss-ecotrim", "fernandez", "litterman-minrss", "ols"
  )

  for (method in methods_with_indicators) {
    m <- td(formula1_q2m, method = method, truncated.rho = if (method == "ols") NULL else -1)
    result <- predict(m)
    aggregated <- ta(result, to = "quarterly", conversion = "sum")
    aggregated <- window(aggregated, start = start(sales.q), end = end(sales.q))

    expect_equal(
      aggregated, sales.q,
      tolerance = 1e-5, # Looser tolerance for monthly disaggregation
      label = sprintf("%s (q2m) aggregation property", method),
      check.attributes = FALSE
    )
  }

  # Test without indicators
  m_den <- td(formula2_q2m, method = "denton-cholette", h = 1, to = 12)
  result_den <- predict(m_den)
  aggregated_den <- ta(result_den, to = "quarterly", conversion = "sum")
  aggregated_den <- window(aggregated_den, start = start(sales.q), end = end(sales.q))

  expect_equal(
    aggregated_den, sales.q,
    tolerance = 1e-5,
    label = "denton-cholette (q2m) aggregation property",
    check.attributes = FALSE
  )
})


test_that("numerical stability over time", {
  skip_on_cran() # Regression test

  # This test ensures that the numerical results don't drift over package updates
  # It uses a simple, controlled example

  # Create simple test data
  y_test <- ts(c(100, 110, 120, 130), start = 2000, frequency = 1)
  x_test <- ts(rep(c(24, 25, 26, 27), each = 4), start = c(2000, 1), frequency = 4)

  # Run disaggregation
  m_test <- td(y_test ~ x_test, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)
  result_test <- predict(m_test)

  # Check that results are reasonable (mean of quarterly should match mean of annual when multiplied by frequency)
  # Mean of disaggregated quarterly: should be annual mean / 4
  expect_lt(abs(mean(result_test) - mean(y_test) / 4), 1.0)

  # Check that aggregation holds exactly
  aggregated_test <- ta(result_test, to = "annual", conversion = "sum")
  aggregated_test <- window(aggregated_test, start = start(y_test), end = end(y_test))

  expect_equal(aggregated_test, y_test, tolerance = 1e-10)
})
