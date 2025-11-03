# Comprehensive Tests for Conversion Types
# =========================================
# These tests verify that all conversion types (sum, average, first, last)
# work correctly across multi-level aggregation chains.
#
# Based on lines 184-248 from the original tests/test-all.R
# These tests can be slow, so they are skipped on CRAN.

library(testthat)
library(tempdisagg)

# Multi-Level Aggregation Chains
# ============================================================================
# Tests verify that aggregation properties hold through multiple levels:
# annual -> monthly -> quarterly -> semi-annual -> annual

test_that("sum conversion preserves aggregation through multi-level chain", {
  skip_on_cran() # Slow comprehensive test

  # Use built-in airmiles dataset
  am <- airmiles

  # Disaggregate annual to monthly
  am_m_sum <- predict(td(am ~ 1, to = "monthly", method = "denton-cholette", conversion = "sum"))

  # Test 1: Monthly aggregates back to annual
  expect_equal(am, ta(am_m_sum, to = "annual", conversion = "sum"))

  # Test 2: Monthly to monthly (identity)
  expect_equal(am_m_sum, ta(am_m_sum, to = 12, conversion = "sum"))

  # Aggregate monthly to quarterly
  am_q_sum <- ta(am_m_sum, to = "quarterly", conversion = "sum")

  # Test 3: Quarterly aggregates back to annual
  expect_equal(am, ta(am_q_sum, to = "annual", conversion = "sum"))

  # Test 4: Quarterly to quarterly (identity)
  expect_equal(am_q_sum, ta(am_q_sum, to = 4, conversion = "sum"))

  # Aggregate quarterly to semi-annual
  am_s_sum <- ta(am_q_sum, to = 2, conversion = "sum")

  # Test 5: Semi-annual aggregates back to annual
  expect_equal(am, ta(am_s_sum, to = "annual", conversion = "sum"))

  # Test 6: Semi-annual to semi-annual (identity)
  expect_equal(am_s_sum, ta(am_s_sum, to = 2, conversion = "sum"))

  # Aggregate semi-annual to annual
  am_y_sum <- ta(am_s_sum, to = "annual", conversion = "sum")

  # Test 7: Annual matches original
  expect_equal(am, ta(am_y_sum, to = "annual", conversion = "sum"))
})


test_that("average conversion preserves aggregation through multi-level chain", {
  skip_on_cran() # Slow comprehensive test

  am <- airmiles

  # Disaggregate annual to monthly (average)
  am_m_average <- predict(td(am ~ 1, to = "monthly", method = "denton-cholette", conversion = "average"))

  # Test 1: Monthly aggregates back to annual
  expect_equal(am, ta(am_m_average, to = "annual", conversion = "average"))

  # Test 2: Monthly to monthly (identity)
  expect_equal(am_m_average, ta(am_m_average, to = 12, conversion = "average"))

  # Aggregate monthly to quarterly
  am_q_average <- ta(am_m_average, to = "quarterly", conversion = "average")

  # Test 3: Quarterly aggregates back to annual
  expect_equal(am, ta(am_q_average, to = "annual", conversion = "average"))

  # Test 4: Quarterly to quarterly (identity)
  expect_equal(am_q_average, ta(am_q_average, to = 4, conversion = "average"))

  # Aggregate quarterly to semi-annual
  am_s_average <- ta(am_q_average, to = 2, conversion = "average")

  # Test 5: Semi-annual aggregates back to annual
  expect_equal(am, ta(am_s_average, to = "annual", conversion = "average"))

  # Test 6: Semi-annual to semi-annual (identity)
  expect_equal(am_s_average, ta(am_s_average, to = 2, conversion = "average"))

  # Aggregate semi-annual to annual
  am_y_average <- ta(am_s_average, to = "annual", conversion = "average")

  # Test 7: Annual matches original
  expect_equal(am, ta(am_y_average, to = "annual", conversion = "average"))
})


test_that("first conversion preserves aggregation through multi-level chain", {
  skip_on_cran() # Slow comprehensive test

  am <- airmiles

  # Disaggregate annual to monthly (first)
  am_m_first <- predict(td(am ~ 1, to = "monthly", method = "denton-cholette", conversion = "first"))

  # Test 1: Monthly aggregates back to annual
  expect_equal(am, ta(am_m_first, to = "annual", conversion = "first"))

  # Test 2: Monthly to monthly (identity)
  expect_equal(am_m_first, ta(am_m_first, to = 12, conversion = "first"))

  # Aggregate monthly to quarterly
  am_q_first <- ta(am_m_first, to = "quarterly", conversion = "first")

  # Test 3: Quarterly aggregates back to annual
  expect_equal(am, ta(am_q_first, to = "annual", conversion = "first"))

  # Test 4: Quarterly to quarterly (identity)
  expect_equal(am_q_first, ta(am_q_first, to = 4, conversion = "first"))

  # Aggregate quarterly to semi-annual
  am_s_first <- ta(am_q_first, to = 2, conversion = "first")

  # Test 5: Semi-annual aggregates back to annual
  expect_equal(am, ta(am_s_first, to = "annual", conversion = "first"))

  # Test 6: Semi-annual to semi-annual (identity)
  expect_equal(am_s_first, ta(am_s_first, to = 2, conversion = "first"))

  # Aggregate semi-annual to annual
  am_y_first <- ta(am_s_first, to = "annual", conversion = "first")

  # Test 7: Annual matches original
  expect_equal(am, ta(am_y_first, to = "annual", conversion = "first"))
})


test_that("last conversion preserves aggregation through multi-level chain", {
  skip_on_cran() # Slow comprehensive test

  am <- airmiles

  # Disaggregate annual to monthly (last)
  am_m_last <- predict(td(am ~ 1, to = "monthly", method = "denton-cholette", conversion = "last"))

  # Test 1: Monthly aggregates back to annual
  expect_equal(am, ta(am_m_last, to = "annual", conversion = "last"))

  # Test 2: Monthly to monthly (identity)
  expect_equal(am_m_last, ta(am_m_last, to = 12, conversion = "last"))

  # Aggregate monthly to quarterly
  am_q_last <- ta(am_m_last, to = "quarterly", conversion = "last")

  # Test 3: Quarterly aggregates back to annual
  expect_equal(am, ta(am_q_last, to = "annual", conversion = "last"))

  # Test 4: Quarterly to quarterly (identity)
  expect_equal(am_q_last, ta(am_q_last, to = 4, conversion = "last"))

  # Aggregate quarterly to semi-annual
  am_s_last <- ta(am_q_last, to = 2, conversion = "last")

  # Test 5: Semi-annual aggregates back to annual
  expect_equal(am, ta(am_s_last, to = "annual", conversion = "last"))

  # Test 6: Semi-annual to semi-annual (identity)
  expect_equal(am_s_last, ta(am_s_last, to = 2, conversion = "last"))

  # Aggregate semi-annual to annual
  am_y_last <- ta(am_s_last, to = "annual", conversion = "last")

  # Test 7: Annual matches original
  expect_equal(am, ta(am_y_last, to = "annual", conversion = "last"))
})


# Conversion Type Consistency Tests
# ============================================================================

test_that("different conversion types produce different but valid results", {
  skip_on_cran()

  am <- window(airmiles, start = 1950, end = 1955) # Smaller subset for speed

  # Disaggregate with all methods
  am_sum <- predict(td(am ~ 1, to = 12, method = "denton-cholette", conversion = "sum"))
  am_avg <- predict(td(am ~ 1, to = 12, method = "denton-cholette", conversion = "average"))
  am_first <- predict(td(am ~ 1, to = 12, method = "denton-cholette", conversion = "first"))
  am_last <- predict(td(am ~ 1, to = 12, method = "denton-cholette", conversion = "last"))

  # All should aggregate back correctly with their respective methods
  expect_equal(am, ta(am_sum, to = 1, conversion = "sum"))
  expect_equal(am, ta(am_avg, to = 1, conversion = "average"))
  expect_equal(am, ta(am_first, to = 1, conversion = "first"))
  expect_equal(am, ta(am_last, to = 1, conversion = "last"))

  # Results should be different
  expect_false(isTRUE(all.equal(am_sum, am_avg)))
  expect_false(isTRUE(all.equal(am_sum, am_first)))
  expect_false(isTRUE(all.equal(am_avg, am_last)))
})


# Test with Actual Package Data
# ============================================================================

test_that("conversion types work with swisspharma data", {
  skip_on_cran()

  data(swisspharma)

  # Test with quarterly sales
  q_data <- window(sales.q, end = c(1980, 4))

  # Aggregate to annual with different methods
  a_sum <- ta(q_data, to = "annual", conversion = "sum")
  a_avg <- ta(q_data, to = "annual", conversion = "average")
  a_first <- ta(q_data, to = "annual", conversion = "first")
  a_last <- ta(q_data, to = "annual", conversion = "last")

  # All should be valid time series
  expect_s3_class(a_sum, "ts")
  expect_s3_class(a_avg, "ts")
  expect_s3_class(a_first, "ts")
  expect_s3_class(a_last, "ts")

  # All should have annual frequency
  expect_equal(frequency(a_sum), 1)
  expect_equal(frequency(a_avg), 1)
  expect_equal(frequency(a_first), 1)
  expect_equal(frequency(a_last), 1)
})
