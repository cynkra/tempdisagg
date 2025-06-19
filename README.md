
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tempdisagg: Methods for Temporal Disaggregation and Interpolation of Time Series

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/christophsax/tempdisagg/branch/main/graph/badge.svg)](https://app.codecov.io/gh/christophsax/tempdisagg?branch=main)
[![R-CMD-check](https://github.com/christophsax/tempdisagg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christophsax/tempdisagg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Temporal disaggregation methods are used to disaggregate or interpolate
a low frequency time series to a higher frequency series, where either
the sum, the average, the first or the last value of the resulting high
frequency series is consistent with the low frequency series. Temporal
disaggregation can be performed with or without one or more high
frequency indicator series. Contains the methods of Chow-Lin,
Santos-Silva-Cardoso, Fernandez, Litterman, Denton and Denton-Cholette.
Supports most R time series classes.

## Installation

To install or update from from
[CRAN](https://cran.r-project.org/package=tempdisagg), run:

``` r
install.packages("tempdisagg")
```

To install the development version:

``` r
# install.packages("remotes")
remotes::install_github("christophsax/tempdisagg")
```

Our article on [temporal disaggregation of time
series](https://journal.r-project.org/archive/2013-2/sax-steiner.pdf) in
the R-Journal describes the package and the theory of temporal
disaggregation in more detail.

Please report bugs on
[Github](https://github.com/christophsax/tempdisagg) or send an
[e-mail](mailto:christoph.sax@gmail.com), thank you!
