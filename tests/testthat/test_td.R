library(testthat)
library(tempdisagg)
library(tsbox)

context("td")

test_that("td works with various to specifications", {
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "quarterly", method = "fast"), "td")
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "quarter", method = "fast"), "td")
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "monthly", method = "fast"), "td")
})

test_that("'mean' can be specified", {
  expect_is(td(ts(c(1, 1, 1, 1)) ~ 1, to = "month", method = "fast", conversion = "mean"), "td")
})

test_that("different lenght", {
  y <- ts(rep(1, 12))
  x1 <- ts(rnorm(12), frequency = 4, start = 1)
  x2 <- ts(rnorm(12), frequency = 4, start = 10)
  expect_message(td(y ~ x1, method = "fast"), "after")
  expect_message(td(y ~ x2, method = "fast"), "before")
})

test_that("dynamic method", {
  expect_is(td(ts(1:8) ~ 1, to = "quarterly", method = "dynamic-maxlog"), "td")
  expect_is(td(ts(1:8) ~ 1, to = "quarterly", method = "dynamic-minrss"), "td")
})

test_that("denton tsbox", {
  expect_is(td(ts_df(ts(1:8)) ~ 1, to = "quarterly", method = "denton-cholette"), "td")
})
