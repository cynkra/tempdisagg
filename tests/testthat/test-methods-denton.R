# Tests for Denton and Denton-Cholette Methods
# ==============================================
# Tests for Denton and Denton-Cholette temporal disaggregation methods.
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


# Denton-Cholette with Different h Values
# ============================================================================

test_that("denton-cholette works with h=0 (original difference)", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton-cholette", h = 0)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  test_aggregation_holds(m, y)
})


test_that("denton-cholette works with h=1 (first difference)", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  test_aggregation_holds(m, y)
})


test_that("denton-cholette works with h=2 (second difference)", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton-cholette", h = 2)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  test_aggregation_holds(m, y)
})


test_that("denton-cholette produces different results for different h values", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  m0 <- td(y ~ 1, to = 4, method = "denton-cholette", h = 0)
  m1 <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1)
  m2 <- td(y ~ 1, to = 4, method = "denton-cholette", h = 2)

  r0 <- predict(m0)
  r1 <- predict(m1)
  r2 <- predict(m2)

  # All should aggregate correctly
  test_aggregation_holds(m0, y)
  test_aggregation_holds(m1, y)
  test_aggregation_holds(m2, y)

  # Results should differ
  expect_false(isTRUE(all.equal(r0, r1)))
  expect_false(isTRUE(all.equal(r1, r2)))
  expect_false(isTRUE(all.equal(r0, r2)))
})


# Proportional vs Additive Criterion
# ============================================================================

test_that("denton-cholette works with proportional criterion", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1, criterion = "proportional")

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("denton-cholette works with additive criterion", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1, criterion = "additive")

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("proportional and additive criteria work correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  m_prop <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1, criterion = "proportional")
  m_add <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1, criterion = "additive")

  r_prop <- predict(m_prop)
  r_add <- predict(m_add)

  # Both should aggregate correctly
  test_aggregation_holds(m_prop, y)
  test_aggregation_holds(m_add, y)

  # Results may be similar or different depending on data characteristics
  # Main requirement is that both produce valid results
  expect_s3_class(r_prop, "ts")
  expect_s3_class(r_add, "ts")
})


# Denton-Cholette with Indicator
# ============================================================================

test_that("denton-cholette works with indicator variable", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  # No intercept model with indicator
  m <- td(y ~ 0 + x, method = "denton-cholette", h = 1)

  expect_s3_class(m, "td")
  expect_no_error(predict(m))
  test_aggregation_holds(m, y)
})


test_that("denton-cholette with indicator differs from without", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  m_ind <- td(y ~ 0 + x, method = "denton-cholette", h = 1)
  m_const <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1)

  r_ind <- predict(m_ind)
  r_const <- predict(m_const)

  # Both should aggregate correctly
  test_aggregation_holds(m_ind, y)
  test_aggregation_holds(m_const, y)

  # Results should differ (indicator should follow x more closely)
  expect_false(isTRUE(all.equal(r_ind, r_const)))
})


# Plain Denton Method
# ============================================================================

test_that("plain denton works with h=0", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton", h = 0)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("plain denton works with h=1", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton", h = 1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("plain denton works with h=2", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton", h = 2)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("denton vs denton-cholette both work correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  m_den <- td(y ~ 1, to = 4, method = "denton", h = 1)
  m_dencho <- td(y ~ 1, to = 4, method = "denton-cholette", h = 1)

  r_den <- predict(m_den)
  r_dencho <- predict(m_dencho)

  # Both should aggregate correctly
  test_aggregation_holds(m_den, y)
  test_aggregation_holds(m_dencho, y)

  # Both should produce valid results
  expect_s3_class(r_den, "ts")
  expect_s3_class(r_dencho, "ts")

  # Results may vary depending on implementation
  expect_false(any(is.na(r_den)))
  expect_false(any(is.na(r_dencho)))
})


# Denton with Different Criteria
# ============================================================================

test_that("plain denton works with proportional criterion", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton", h = 1, criterion = "proportional")

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


test_that("plain denton works with additive criterion", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  m <- td(y ~ 1, to = 4, method = "denton", h = 1, criterion = "additive")

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y)
})


# Quarter to Month Disaggregation
# ============================================================================

test_that("denton-cholette works for quarter to month disaggregation", {
  skip_on_cran()

  y <- window(sales.q, end = c(1985, 4))
  m <- td(y ~ 1, to = 12, method = "denton-cholette", h = 1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y, tolerance = 1e-5)
})


test_that("denton works for quarter to month disaggregation", {
  skip_on_cran()

  y <- window(sales.q, end = c(1985, 4))
  m <- td(y ~ 1, to = 12, method = "denton", h = 1)

  expect_s3_class(m, "td")
  test_aggregation_holds(m, y, tolerance = 1e-5)
})


# Comprehensive Matrix of Methods
# ============================================================================

test_that("all denton method combinations work", {
  skip_on_cran()

  y <- window(sales.a, end = 1980) # Smaller data for speed

  # Matrix of all combinations
  methods <- c("denton", "denton-cholette")
  h_values <- c(0, 1, 2)
  criteria <- c("proportional", "additive")

  for (method in methods) {
    for (h in h_values) {
      for (criterion in criteria) {
        m <- td(y ~ 1, to = 4, method = method, h = h, criterion = criterion)
        expect_s3_class(m, "td")
        test_aggregation_holds(m, y, tolerance = 1e-7)
      }
    }
  }
})
