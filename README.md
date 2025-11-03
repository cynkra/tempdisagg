# tempdisagg: Methods for Temporal Disaggregation and Interpolation of Time Series

<!-- badges: start -->

[![smoke-test](https://github.com/cynkra/tempdisagg/actions/workflows/smoke-test.yaml/badge.svg)](https://github.com/cynkra/tempdisagg/actions/workflows/smoke-test.yaml)
[![R-CMD-check](https://github.com/cynkra/tempdisagg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cynkra/tempdisagg/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/cynkra/tempdisagg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/cynkra/tempdisagg?branch=main)
[![pkgdown](https://github.com/cynkra/tempdisagg/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/cynkra/tempdisagg/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

Temporal disaggregation methods are used to disaggregate or interpolate
a low frequency time series to a higher frequency series, where either
the sum, the average, the first or the last value of the resulting high
frequency series is consistent with the low frequency series. Temporal
disaggregation can be performed with or without one or more high
frequency indicator series. Contains the methods of **Chow-Lin**,
**Santos-Silva-Cardoso**, **Fernandez**, **Litterman**, **Denton** and **Denton-Cholette**.
Supports most R time series classes.

## Installation

To install or update from from
[CRAN](https://cran.r-project.org/package=tempdisagg), run:

``` r
install.packages("tempdisagg")
```

To install the development version:

``` r
# install.packages("pak")
pak::pak("cynkra/tempdisagg")
```

Our article on [temporal disaggregation of time
series](https://journal.r-project.org/archive/2013-2/sax-steiner.pdf) in
the R-Journal describes the package and the theory of temporal
disaggregation in more detail.

Please report bugs on [Github](https://github.com/cynkra/tempdisagg). Thank you!
