# noinst/

Tools to generate test code from .RData files. Not included in package.

## Files

- `td0.24.1_R3.2.3_64bit.RData` - Reference results from v0.24.1 for regression testing
- `data_input_swisspharma.RData` - Historical test data (redundant with `data/swisspharma.RData`)
- `00_functions.R` - Helper functions for legacy tests
- `extract_test_data.R` - Example: extract data to R code using `constructive`
- `generate_test_code.R` - Functions to generate test fixtures

## Usage

Regenerate test fixtures:
```r
source("noinst/generate_test_code.R")
generate_test_fixtures()
```

Old tests loaded .RData files and only ran on CI. New tests use inline R code and `skip_on_cran()`, so they run locally and are better organized.
