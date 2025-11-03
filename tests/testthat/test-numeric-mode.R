library(testthat)
library(tempdisagg)

context("numeric mode")

test_that("numeric mode works as expected", {
  y <- c(2, 2, 2, 2, 2, 2, 2, 2)
  expect_error(td(y ~ 1, to = "monthly"))

  y <- c(2, 2, 2, 3, 2, 5, 2, 2)
  m0 <- predict(td(y ~ 1, to = 12))

  y.ts <- ts(y, start = 2000)
  m1 <- predict(td(y.ts ~ 1, to = "monthly"))

  expect_equal(m0, as.numeric(m1))
})

test_that("numeric mode works with different methods", {
  y <- c(100, 150, 200, 250, 300)
  x <- rep(c(20, 25, 30, 35), 5)

  # Test different methods work in numeric mode
  m_chow <- td(y ~ x, to = 4, method = "chow-lin-maxlog")
  m_fern <- td(y ~ x, to = 4, method = "fernandez")
  m_unif <- td(y ~ 1, to = 4, method = "uniform")

  expect_type(predict(m_chow), "double")
  expect_type(predict(m_fern), "double")
  expect_type(predict(m_unif), "double")
})

test_that("numeric mode preserves aggregation property", {
  # Use simpler data that won't create singular matrix
  y <- c(120, 240, 360, 480, 600)
  x <- seq(10, 200, length.out = 20)

  m <- td(y ~ x, to = 4, method = "chow-lin-maxlog")
  result <- predict(m)

  # Aggregate back to original frequency
  aggregated <- colSums(matrix(result, nrow = 4))

  expect_equal(aggregated, y, tolerance = 1e-7)
})

test_that("numeric mode handles edge cases", {
  # Longer series to avoid degrees of freedom issues
  y_test <- c(10, 20, 30, 40)
  x_test <- seq(2, 48, length.out = 16)

  m_test <- td(y_test ~ x_test, to = 4, method = "chow-lin-maxlog")
  expect_length(predict(m_test), length(x_test))

  # Constant series (uniform distribution)
  y_const <- c(100, 100, 100, 100)

  m_const <- td(y_const ~ 1, to = 12, method = "uniform")
  result_const <- predict(m_const)

  expect_type(result_const, "double")
  # Should distribute evenly for uniform method
  expect_true(sd(result_const) < 1)
})

test_that("numeric mode handles conversion parameter", {
  # Test with different conversion types
  y <- c(100, 200, 300, 400)
  x <- c(rep(25, 4), rep(50, 4), rep(75, 4), rep(100, 4))

  # Sum conversion (default for flow variables)
  m_sum <- td(y ~ x, to = 4, conversion = "sum", method = "fernandez")
  result_sum <- predict(m_sum)

  # Average conversion (for stock variables)
  m_avg <- td(y ~ x, to = 4, conversion = "average", method = "fernandez")
  result_avg <- predict(m_avg)

  expect_type(result_sum, "double")
  expect_type(result_avg, "double")

  # Results should differ based on conversion type
  expect_false(isTRUE(all.equal(result_sum, result_avg)))
})
