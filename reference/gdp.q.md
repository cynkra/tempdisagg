# Gross Domestic Product

Qarterly real GDP, not seasonally adjusted, in millions of Swiss Francs
(2010 prices).

## Format

A `data.frame`.

## Source

State Secretariat for Economic Affairs (SECO).

## Examples

``` r
if (FALSE) { # \dontrun{
# recreate the series with latest data
library(tsbox)
library(dplyr)
library(dataseries)
library(imputeTS)
dataseries::ds("ch_seco_gdp.nsa.real.gdp") %>%
  ts_default() %>%
  ts_span(start = 2005)
} # }
```
