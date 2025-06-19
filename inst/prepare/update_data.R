library(tidyverse)
library(tsbox)

load("data/swisspharma.RData")

gdp.q <-
  dataseries::ds("ch_seco_gdp.nsa.real.gdp") %>%
  ts_tbl() %>%
  ts_default() %>%
  ts_span(start = 2005) %>%
  as.data.frame()

spi.d <-
  dataseries::ds("ch_snb_capchstocki.gdr") %>%
  ts_tbl() %>%
  ts_default() %>%
  ts_regular() %>%
  imputeTS::na_interpolation(option = "spline") %>%
  ts_span(start = 2005) %>%
  as.data.frame()

obj <- load("data/swisspharma.RData")

save(sales.a, sales.q, exports.q, imports.q, exports.m, gdp.q, spi.d, file = "data/tempdisagg.RData", version = 2)

