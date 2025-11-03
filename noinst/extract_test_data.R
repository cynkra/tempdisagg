# Example Script: Extract Test Data from .RData Files
# =====================================================
# This script demonstrates how to use the constructive package to generate
# R code representations of data objects from .RData files.
#
# The generated code can then be copied into test files, eliminating the need
# for external data dependencies.

library(constructive)

# Load the reference data
# ============================================================================
load("noinst/data_input_swisspharma.RData")
load("noinst/td0.24.1_R3.2.3_64bit.RData")

# Example 1: Generate code for a simple time series
# ============================================================================
cat("\n=== Example 1: sales.a (full object) ===\n")
construct(sales.a)

cat("\n\n=== Example 1b: sales.a (first 5 values only) ===\n")
sales.a.mini <- window(sales.a, end = 1979)
construct(sales.a.mini)


# Example 2: Generate code for quarterly exports (subset)
# ============================================================================
cat("\n\n=== Example 2: exports.q (first 2 years = 8 quarters) ===\n")
exports.q.mini <- window(exports.q, end = c(1976, 4))
construct(exports.q.mini)


# Example 3: Extract specific reference values from old results
# ============================================================================
cat("\n\n=== Example 3: Reference values for regression tests ===\n")

# Get first 4 values from a few key methods
ref_subset <- list(
  dencho_p_1 = as.numeric(r$y2q[1:4, "dencho_p_1"]),
  cl_rss_R = as.numeric(r$y2q[1:4, "cl_rss_R"]),
  fer = as.numeric(r$y2q[1:4, "fer"])
)

construct(ref_subset)


# Example 4: Generate code with styling
# ============================================================================
cat("\n\n=== Example 4: Formatted with styler ===\n")

code_text <- construct(sales.a.mini, pipe = "base")
code_chr <- capture.output(code_text)
styled_code <- styler::style_text(code_chr)
cat(paste(styled_code, collapse = "\n"))


# Example 5: Create a complete test data fixture
# ============================================================================
cat("\n\n=== Example 5: Complete test fixture ===\n")

test_fixture <- list(
  # Input data (shortened for testing)
  sales_annual = window(sales.a, start = 1975, end = 1979),
  exports_quarterly = window(exports.q, start = c(1975, 1), end = c(1979, 4)),

  # Reference values for key methods (first 4 quarters only)
  reference = list(
    dencho_p_1 = as.numeric(r$y2q[1:4, "dencho_p_1"]),
    cl_rss_R = as.numeric(r$y2q[1:4, "cl_rss_R"])
  )
)

construct(test_fixture)


# Verification: Test that constructed code works
# ============================================================================
cat("\n\n=== Verification ===\n")

# Reconstruct sales.a from generated code and compare
sales_reconstructed <- ts(
  c(
    136.7023, 151.0561, 156.1824, 157.2078, 162.334, 174.5098,
    188.6881, 199.8403, 209.3044, 217.9514, 226.4858, 236.3158,
    247.2106, 259.5633, 270.917, 281.0579, 290.1869, 299.3203,
    307.0203, 316.581, 326.9476, 335.2306, 343.9407, 353.0683,
    363.5683, 375.2936, 385.3843, 395.0016, 406.2823, 416.0856,
    425.9923, 436.3003, 446.8336, 458.3336, 469.5836, 481.5003
  ),
  start = 1975, frequency = 1
)

cat("sales.a matches reconstructed:", all.equal(sales.a, sales_reconstructed), "\n")


# Notes
# ============================================================================
cat("\n\n=== Notes ===\n")
cat("1. Use constructive::construct() to generate R code from objects\n")
cat("2. Use styler::style_text() to format the generated code nicely\n")
cat("3. Extract subsets of large objects to keep test files manageable\n")
cat("4. Verify that constructed code recreates the original object\n")
cat("5. For tests: prefer data(swisspharma) for built-in data\n")
cat("6. Only inline small custom fixtures or reference values\n")
