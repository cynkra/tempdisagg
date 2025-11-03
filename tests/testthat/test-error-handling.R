
library(testthat)
library(tempdisagg)

# Load test data
data(swisspharma)


# SubRegressionBased Error Conditions
# ----------------------------------------------------------------------------

test_that("td() errors on unsupported method", {
  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  expect_error(
    td(y ~ x, method = "nonexistent-method"),
    "method does not exist"
  )
})

test_that("td() handles near-singular matrix", {
  skip_on_cran()

  # Create data with highly correlated (but not perfectly singular) indicators
  y <- window(sales.a, start = 1980, end = 1985)
  x1 <- window(exports.q, start = c(1980, 1), end = c(1985, 4))
  # Create highly correlated but not identical series
  x2 <- x1 + ts(rnorm(length(x1), sd = 0.1), start = start(x1), frequency = frequency(x1))

  # Should complete without error even with high correlation
  m <- td(y ~ x1 + x2, method = "chow-lin-maxlog")
  expect_s3_class(m, "td")
})

test_that("td() handles rho truncation correctly", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  # With truncated.rho = -1, rho should not go below -1
  m <- td(y ~ x, method = "chow-lin-maxlog", truncated.rho = -1)

  expect_gte(m$rho, -1)
})

test_that("td() works with fixed rho methods", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  # Fixed rho should use specified value
  m <- td(y ~ x, method = "chow-lin-fixed", fixed.rho = 0.5)

  expect_equal(m$rho, 0.5)
  expect_s3_class(m, "td")
})

test_that("td() handles litterman fixed rho", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  m <- td(y ~ x, method = "litterman-fixed", fixed.rho = 0.7)

  expect_equal(m$rho, 0.7)
  expect_s3_class(m, "td")
})

test_that("td() handles dynamic fixed rho", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)
  x <- window(exports.q, end = c(1985, 4))

  m <- td(y ~ x, method = "dynamic-fixed", fixed.rho = 0.3)

  expect_equal(m$rho, 0.3)
  expect_s3_class(m, "td")
})


# SubDenton Error Conditions
# ----------------------------------------------------------------------------

test_that("denton methods error with multiple RHS variables", {
  y <- window(sales.a, end = 1985)
  x1 <- window(exports.q, end = c(1985, 4))
  x2 <- window(imports.q, end = c(1985, 4))

  expect_error(
    td(y ~ x1 + x2, method = "denton"),
    "only one series allowed"
  )

  expect_error(
    td(y ~ x1 + x2, method = "denton-cholette"),
    "only one series allowed"
  )
})

test_that("denton methods error with invalid criterion", {
  y <- window(sales.a, end = 1985)

  expect_error(
    td(y ~ 1, to = 4, method = "denton", criterion = "invalid"),
    "criterion for Denton methods must be additive or proportional"
  )
})

test_that("denton methods error with invalid h specification", {
  y <- window(sales.a, end = 1985)

  expect_error(
    td(y ~ 1, to = 4, method = "denton", h = -1),
    "wrong specification of h"
  )
})

test_that("uniform method is special case of denton", {
  skip_on_cran()

  y <- window(sales.a, end = 1985)

  # Uniform should work and be equivalent to denton with h=0, additive
  m_uniform <- td(y ~ 1, to = 4, method = "uniform")
  m_denton <- td(y ~ 1, to = 4, method = "denton", h = 0, criterion = "additive")

  expect_s3_class(m_uniform, "td")
  # Method name stays "uniform" but has same properties as denton h=0, additive
  expect_equal(m_uniform$h, 0)
  expect_equal(m_uniform$criterion, "additive")

  # Results should be identical
  expect_equal(predict(m_uniform), predict(m_denton))
})
