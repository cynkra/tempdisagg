library(testthat)
library(tempdisagg)

context("output functions")

m <- td(ts(c(1, 1, 1, 1)) ~ 1, to = 4, method = "fast")

test_that("plot works", {
  plot(m)
  expect_null(NULL)
})

test_that("print works", {
  expect_output(print(m))
})
