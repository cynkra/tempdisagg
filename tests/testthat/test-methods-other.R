# Tests for Other Disaggregation Methods
# ========================================
# Tests for Fernandez, Litterman, OLS, and Uniform methods.
# These are comprehensive tests that are skipped on CRAN.

library(testthat)
library(tempdisagg)

# Load test data
data(swisspharma)

# Helper function for aggregation tests
test_aggregation_holds <- function(model, original_data, tolerance = 1e-7) {
  disagg <- predict(model)
  aggregated <- ta(disagg, to = frequency(original_data), conversion = "sum")
  aggregated_windowed <- window(aggregated, start = start(original_data), end = end(original_data))
  expect_equal(aggregated_windowed, original_data, tolerance = tolerance)
}


# Fernandez Method
# ============================================================================

test_that("fernandez method works correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m <- td(y ~ x1 + x2, method = "fernandez", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  test_aggregation_holds(m, y)
})


test_that("fernandez works with single indicator", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  m <- td(y ~ x, method = "fernandez", truncated.rho = -1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("fernandez differs from chow-lin", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m_fer <- td(y ~ x1 + x2, method = "fernandez", truncated.rho = -1)
  m_cl <- td(y ~ x1 + x2, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)

  r_fer <- predict(m_fer)
  r_cl <- predict(m_cl)

  # Both should aggregate correctly
  test_aggregation_holds(m_fer, y)
  test_aggregation_holds(m_cl, y)

  # Results should be similar but not identical
  expect_gt(cor(as.numeric(r_fer), as.numeric(r_cl)), 0.95)
  expect_false(isTRUE(all.equal(r_fer, r_cl, tolerance = 1e-5)))
})


# Litterman Methods
# ============================================================================

test_that("litterman-minrss works correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m <- td(y ~ x1 + x2, method = "litterman-minrss", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  test_aggregation_holds(m, y)
})


test_that("litterman-maxlog works correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m <- td(y ~ x1 + x2, method = "litterman-maxlog", truncated.rho = -1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  test_aggregation_holds(m, y)
})


test_that("litterman-fixed works with specified rho", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m <- td(y ~ x1 + x2, method = "litterman-fixed", fixed.rho = 0.6)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))

  test_aggregation_holds(m, y)
  expect_equal(m$rho, 0.6)
})


test_that("litterman minrss and maxlog produce different results", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m_minrss <- td(y ~ x1 + x2, method = "litterman-minrss", truncated.rho = -1)
  m_maxlog <- td(y ~ x1 + x2, method = "litterman-maxlog", truncated.rho = -1)

  r_minrss <- predict(m_minrss)
  r_maxlog <- predict(m_maxlog)

  # Both should aggregate correctly
  test_aggregation_holds(m_minrss, y)
  test_aggregation_holds(m_maxlog, y)

  # Results should be similar but not identical
  expect_gt(cor(as.numeric(r_minrss), as.numeric(r_maxlog)), 0.95)
  expect_false(isTRUE(all.equal(r_minrss, r_maxlog, tolerance = 1e-5)))
})


# OLS Method
# ============================================================================

test_that("ols method works correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m <- td(y ~ x1 + x2, method = "ols")

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  expect_no_error(summary(m))

  test_aggregation_holds(m, y)
})


test_that("ols works with single indicator", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  m <- td(y ~ x, method = "ols")

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("ols is simpler than chow-lin", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  m_ols <- td(y ~ x1 + x2, method = "ols")
  m_cl <- td(y ~ x1 + x2, method = "chow-lin-minrss-ecotrim", truncated.rho = -1)

  # OLS should not have rho parameter (or rho should be 0)
  expect_true(is.null(m_ols$rho) || m_ols$rho == 0)

  # Chow-Lin should have estimated rho
  expect_false(is.null(m_cl$rho))

  # Both should work
  test_aggregation_holds(m_ols, y)
  test_aggregation_holds(m_cl, y)
})


# Uniform Method
# ============================================================================

test_that("uniform method works correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  m <- td(y ~ 1, to = 4, method = "uniform")

  expect_s3_class(m, "td")
  expect_no_error(predict(m))

  test_aggregation_holds(m, y)
})


test_that("uniform distributes values evenly", {
  skip_on_cran()

  # Simple test case
  y <- ts(c(100, 100, 100), start = 2000, frequency = 1)

  m <- td(y ~ 1, to = 4, method = "uniform")
  result <- predict(m)

  # Each quarter should be 25 (100/4)
  expect_equal(
    as.numeric(window(result, start = c(2000, 1), end = c(2000, 4))),
    rep(25, 4)
  )
})


test_that("uniform differs from denton methods", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  m_uni <- td(y ~ 1, to = 4, method = "uniform")
  m_den <- td(y ~ 1, to = 4, method = "denton", h = 1)

  r_uni <- predict(m_uni)
  r_den <- predict(m_den)

  # Both should aggregate correctly
  test_aggregation_holds(m_uni, y)
  test_aggregation_holds(m_den, y)

  # Results should differ (uniform is flat within each year)
  expect_false(isTRUE(all.equal(r_uni, r_den)))
})


# Quarter to Month Disaggregation
# ============================================================================

test_that("fernandez works for quarter to month disaggregation", {
  skip_on_cran()

  y <- window(sales.q, end = c(1985, 4))
  x1 <- window(exports.m, end = c(1985, 12))
  x2 <- window(imports.m, end = c(1985, 12))

  m <- td(y ~ x1 + x2, method = "fernandez", truncated.rho = -1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y, tolerance = 1e-5)
})


test_that("litterman works for quarter to month disaggregation", {
  skip_on_cran()

  y <- window(sales.q, end = c(1985, 4))
  x1 <- window(exports.m, end = c(1985, 12))
  x2 <- window(imports.m, end = c(1985, 12))

  m <- td(y ~ x1 + x2, method = "litterman-minrss", truncated.rho = -1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y, tolerance = 1e-5)
})


test_that("ols works for quarter to month disaggregation", {
  skip_on_cran()

  y <- window(sales.q, end = c(1985, 4))
  x1 <- window(exports.m, end = c(1985, 12))
  x2 <- window(imports.m, end = c(1985, 12))

  m <- td(y ~ x1 + x2, method = "ols")

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y, tolerance = 1e-5)
})
