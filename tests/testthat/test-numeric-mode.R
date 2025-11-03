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
