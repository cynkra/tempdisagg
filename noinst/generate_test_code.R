# Helper Functions to Generate Test Code from .RData Files
# ===========================================================
# These functions extract data from .RData files and generate clean,
# readable R code that can be copied into test files.

library(constructive)

#' Generate R code to recreate a time series object
#'
#' @param ts_obj A ts object
#' @param name Optional name for the object (for documentation)
#' @param max_values Maximum number of values to include (NULL for all)
#' @param digits Number of digits for rounding (default 10 for high precision)
#' @return Character vector of formatted R code
generate_ts_code <- function(ts_obj, name = NULL, max_values = NULL, digits = 10) {
  if (!is.null(max_values) && length(ts_obj) > max_values) {
    ts_obj <- window(ts_obj, end = time(ts_obj)[max_values])
  }

  # Extract components
  values <- as.numeric(ts_obj)
  freq <- frequency(ts_obj)
  start_time <- start(ts_obj)

  # Round values for cleaner output
  values <- round(values, digits)

  # Format the values nicely - break into lines of ~4 values
  values_per_line <- 4
  value_lines <- split(values, ceiling(seq_along(values) / values_per_line))
  value_strings <- sapply(value_lines, function(x) paste(x, collapse = ", "))

  # Format start parameter
  start_str <- if (length(start_time) == 1) {
    as.character(start_time)
  } else {
    paste0("c(", paste(start_time, collapse = ", "), ")")
  }

  # Build the code
  code <- c(
    if (!is.null(name)) paste0("# ", name),
    "ts(",
    "  c(",
    paste0("    ", value_strings, ifelse(seq_along(value_strings) < length(value_strings), ",", "")),
    "  ),",
    paste0("  start = ", start_str, ","),
    paste0("  frequency = ", freq),
    ")"
  )

  return(code)
}


#' Generate R code for a list of reference values
#'
#' @param value_list Named list of numeric vectors
#' @param digits Number of digits for rounding
#' @return Character vector of formatted R code
generate_list_code <- function(value_list, digits = 10) {
  # Use constructive but with cleaner numeric formatting
  code <- capture.output(construct(lapply(value_list, function(x) round(x, digits))))
  # Format with styler
  styled <- styler::style_text(code)
  return(as.character(styled))
}


#' Extract reference values from td0.24.1 file for specific methods
#'
#' @param methods Character vector of method names
#' @param n_values Number of values to extract from each method
#' @param conversion Either "y2q" or "q2m"
#' @return Named list of reference values
extract_reference_values <- function(methods, n_values = 10, conversion = "y2q") {
  # Load the reference data
  env <- new.env()
  load("noinst/td0.24.1_R3.2.3_64bit.RData", envir = env)

  # Get the r list and extract the appropriate conversion
  r <- env$r
  result_matrix <- r[[conversion]]

  # Extract values for each method
  ref_values <- lapply(methods, function(method) {
    if (method %in% colnames(result_matrix)) {
      as.numeric(result_matrix[1:min(n_values, nrow(result_matrix)), method])
    } else {
      warning("Method ", method, " not found in reference data")
      NULL
    }
  })
  names(ref_values) <- methods

  # Remove NULL entries
  ref_values[!sapply(ref_values, is.null)]
}


#' Complete workflow: Extract and format test fixtures
#'
#' @param output_file File to write the generated code to
generate_test_fixtures <- function(output_file = "tests/testthat/helper-fixtures.R") {
  # Load both data files
  load("noinst/data_input_swisspharma.RData")
  load("noinst/td0.24.1_R3.2.3_64bit.RData")

  # Helper functions from 00_functions.R
  source("noinst/00_functions.R")

  output <- c(
    "# Test Fixtures and Helper Functions",
    "# ===================================",
    "# This file contains test data and helper functions.",
    "# Generated automatically from noinst/ data files.",
    "# DO NOT EDIT BY HAND - regenerate using noinst/generate_test_code.R",
    "",
    "# Helper Functions",
    "# ---------------",
    ""
  )

  # Add the helper functions
  output <- c(
    output,
    "# Function to run all temporal disaggregation methods",
    readLines("noinst/00_functions.R"),
    "",
    "# Reference Values for Regression Tests",
    "# --------------------------------------",
    "# These are expected outputs from tempdisagg v0.24.1 for comparison",
    ""
  )

  # Extract key reference values (just a few methods, not all 34)
  key_methods <- c(
    "dencho_p_1", "dencho_p_2",
    "cl_rss_R", "cl_log",
    "fer", "lit_rss"
  )

  output <- c(
    output,
    "# Year to quarter conversion - first 10 values for key methods",
    "reference_y2q <- ",
    generate_list_code(extract_reference_values(key_methods, n_values = 10, conversion = "y2q")),
    ""
  )

  output <- c(
    output,
    "# Quarter to month conversion - first 10 values for key methods",
    "reference_q2m <- ",
    generate_list_code(extract_reference_values(key_methods, n_values = 10, conversion = "q2m")),
    ""
  )

  # Write to file
  writeLines(output, output_file)
  cat("Generated test fixtures written to:", output_file, "\n")

  # Format with styler
  styler::style_file(output_file)
  cat("File formatted with styler\n")
}


# Example usage
# =============
if (FALSE) {
  # Generate ts code
  load("noinst/data_input_swisspharma.RData")

  cat("=== Small sales.a subset ===\n")
  cat(generate_ts_code(sales.a, "Annual sales", max_values = 10), sep = "\n")

  cat("\n\n=== Reference values ===\n")
  ref_vals <- extract_reference_values(c("dencho_p_1", "cl_rss_R", "fer"), n_values = 5)
  cat(generate_list_code(ref_vals), sep = "\n")

  cat("\n\n=== Generate complete fixtures file ===\n")
  # generate_test_fixtures()
}
