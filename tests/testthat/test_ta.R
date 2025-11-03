test_that("ta works", {
  x <- ts(rep(1, 23), frequency = 12, start = c(2000, 2))
  expect_equal(ta(x, to = "annual", conversion = "sum"), ts(12, start = 2001))
})
