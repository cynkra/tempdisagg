# SPI Swiss Performance Index

Daily values of stock market index.

## Format

A `data.frame`.

## Source

Swiss National Bank (SNB)

## Examples

``` r
if (FALSE) { # \dontrun{
# recreate the series with latest data
library(tsbox)
library(dplyr)
library(dataseries)
library(imputeTS)
dataseries::ds("ch_snb_capchstocki.gdr") %>%
  ts_default() %>%
  ts_regular() %>%
  imputeTS::na_interpolation(option = "spline") %>%
  ts_span(start = 2005)
} # }
```
