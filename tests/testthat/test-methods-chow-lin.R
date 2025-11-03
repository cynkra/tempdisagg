# Tests for Chow-Lin Methods
# ===========================
# Tests for Chow-Lin temporal disaggregation methods.
# These are comprehensive tests that are skipped on CRAN.

library(testthat)
library(tempdisagg)

# Load test data
data(swisspharma)

# Helper function for aggregation tests
test_aggregation_holds <- function(model, original_data, tolerance = 1e-7) {
  disagg <- predict(model)
  aggregated <- ta(disagg, to = frequency(original_data), conversion = "sum")
  # Window to match original data period
  aggregated_windowed <- window(aggregated, start = start(original_data), end = end(original_data))
  expect_equal(aggregated_windowed, original_data, tolerance = tolerance)
}


# Chow-Lin MinRSS Variants
# ============================================================================

test_that("chow-lin-minrss-ecotrim works correctly", {
  skip_on_cran()

  # Year to quarter disaggregation
  m <- td(sales.a ~ exports.q + imports.q, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  # Test aggregation property
  test_aggregation_holds(m, sales.a, tolerance = 1e-7)
})


test_that("chow-lin-minrss-quilis works correctly", {
  skip_on_cran()

  # Year to quarter disaggregation
  m <- td(sales.a ~ exports.q + imports.q, method = "chow-lin-minrss-quilis", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  # Test aggregation property
  test_aggregation_holds(m, sales.a, tolerance = 1e-7)
})


test_that("chow-lin-minrss methods produce similar but not identical results", {
  skip_on_cran()

  # Use smaller data subset for speed
  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m_ecotrim <- td(y ~ x1 + x2, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)
  m_quilis <- td(y ~ x1 + x2, method = "chow-lin-minrss-quilis", truncated.rho = -1)

  r_ecotrim <- predict(m_ecotrim)
  r_quilis <- predict(m_quilis)

  # Both should aggregate correctly
  test_aggregation_holds(m_ecotrim, y)
  test_aggregation_holds(m_quilis, y)

  # Results should be similar (correlation > 0.99) but not identical
  expect_gt(cor(as.numeric(r_ecotrim), as.numeric(r_quilis)), 0.99)
  expect_false(isTRUE(all.equal(r_ecotrim, r_quilis, tolerance = 1e-10)))
})


# Chow-Lin MaxLog
# ============================================================================

test_that("chow-lin-maxlog works correctly", {
  skip_on_cran()

  m <- td(sales.a ~ exports.q + imports.q, method = "chow-lin-maxlog", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  # Test aggregation property
  test_aggregation_holds(m, sales.a, tolerance = 1e-7)
})


test_that("chow-lin-maxlog differs from minrss methods", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m_maxlog <- td(y ~ x1 + x2, method = "chow-lin-maxlog", truncated.rho = -1)
  m_minrss <- td(y ~ x1 + x2, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)

  r_maxlog <- predict(m_maxlog)
  r_minrss <- predict(m_minrss)

  # Both should aggregate correctly
  test_aggregation_holds(m_maxlog, y)
  test_aggregation_holds(m_minrss, y)

  # Results should be similar but not identical
  expect_gt(cor(as.numeric(r_maxlog), as.numeric(r_minrss)), 0.95)
  expect_false(isTRUE(all.equal(r_maxlog, r_minrss, tolerance = 1e-5)))
})


# Chow-Lin Fixed Rho
# ============================================================================

test_that("chow-lin-fixed works with specified rho", {
  skip_on_cran()

  # Test with rho = 0.6
  m <- td(sales.a ~ exports.q + imports.q, method = "chow-lin-fixed", fixed.rho = 0.6)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  # Test aggregation property
  test_aggregation_holds(m, sales.a, tolerance = 1e-7)

  # Check that rho is actually 0.6
  expect_equal(m$rho, 0.6)
})


test_that("chow-lin-fixed works with different rho values", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m_rho_0 <- td(y ~ x1 + x2, method = "chow-lin-fixed", fixed.rho = 0.0)
  m_rho_05 <- td(y ~ x1 + x2, method = "chow-lin-fixed", fixed.rho = 0.5)
  m_rho_09 <- td(y ~ x1 + x2, method = "chow-lin-fixed", fixed.rho = 0.9)

  # All should work and aggregate correctly
  test_aggregation_holds(m_rho_0, y)
  test_aggregation_holds(m_rho_05, y)
  test_aggregation_holds(m_rho_09, y)

  # Results should differ based on rho
  r0 <- predict(m_rho_0)
  r05 <- predict(m_rho_05)
  r09 <- predict(m_rho_09)

  expect_false(isTRUE(all.equal(r0, r05)))
  expect_false(isTRUE(all.equal(r05, r09)))
})


# Quarter to Month Disaggregation
# ============================================================================

test_that("chow-lin methods work for quarter to month disaggregation", {
  skip_on_cran()

  # Use smaller data subset
  y <- window(sales.q, end = c(1985, 4))
  x1 <- window(exports.m, end = c(1985, 12))
  x2 <- window(imports.m, end = c(1985, 12))

  m <- td(y ~ x1 + x2, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))

  # Test aggregation property (with looser tolerance for monthly data)
  test_aggregation_holds(m, y, tolerance = 1e-5)
})


# Edge Cases and Robustness
# ============================================================================

test_that("chow-lin works with single indicator", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  m <- td(y ~ x, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("chow-lin works with intercept-only model", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  # Note: intercept-only should probably use a different method like denton,
  # but chow-lin should handle it gracefully or error informatively
  expect_error(
    td(y ~ 1, method = "chow-lin-minrss-ecotrim", truncated.rho = -1),
    NA # NA means we expect no error, or we might expect a specific error
  )
})
