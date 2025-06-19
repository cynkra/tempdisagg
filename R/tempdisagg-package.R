#' Methods for Temporal Disaggregation and Interpolation of Time Series
#'
#' @description Temporal disaggregation methods are used to disaggregate or
#'   interpolate a low frequency time series to higher frequency series, where
#'   either the sum, the average, the first or the last value of the resulting
#'   high frequency series is consistent with the low frequency series. Temporal
#'   disaggregation can be performed with or without one or more high frequency
#'   indicator series.
#'
#'   A good way to start is the introductory vignette:
#'
#'   `vignette("intro", "tempdisagg")`
#'
#'   Our article on temporal disaggregation of time series
#'   (\doi{10.32614/RJ-2013-028}) in the R-Journal describes the
#'   package and the theory of temporal disaggregation in more detail.
#'
#' @name tempdisagg-package
#' @aliases tempdisagg
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}, Peter Steiner
#' @keywords package
#' @seealso [td()] for more information on usage.
NULL


#' Trade and Sales of Chemical and Pharmaceutical Industry
#'
#' This data set contains the monthly and quarterly imports and exports of the
#' chemical and pharmaceutical industry in Switzerland (in in millions of Swiss
#' Francs) as well as their quarterly and annual sales (Index).
#'
#' @docType data
#'
#' @format Each time series is an object of class `"ts"`. The number of
#' observations depends on the frequency.
#' @source Import and Export Data are from the Swiss Federal Customs
#' Administration. Sales Data are from the Swiss Federal Statistical Office.
#'
#' @name exports.m
#' @aliases exports.q imports.q sales.a sales.q
#' @keywords datasets
NULL


#' Gross Domestic Product
#'
#' Qarterly real GDP, not seasonally adjusted, in millions of Swiss Francs
#' (2010 prices).
#'
#' @docType data
#'
#' @format A `data.frame`.
#' @source  State Secretariat for Economic Affairs (SECO).
#'
#' @name gdp.q
#' @keywords datasets
#' @examples
#' \dontrun{
#' # recreate the series with latest data
#' library(tsbox)
#' library(dplyr)
#' library(dataseries)
#' library(imputeTS)
#' dataseries::ds("ch_seco_gdp.nsa.real.gdp") %>%
#'   ts_default() %>%
#'   ts_span(start = 2005)
#' }
NULL


#' SPI Swiss Performance Index
#'
#' Daily values of stock market index.
#'
#' @docType data
#'
#' @format A `data.frame`.
#' @source Swiss National Bank (SNB)
#'
#' @name spi.d
#' @keywords datasets
#' @examples
#' \dontrun{
#' # recreate the series with latest data
#' library(tsbox)
#' library(dplyr)
#' library(dataseries)
#' library(imputeTS)
#' dataseries::ds("ch_snb_capchstocki.gdr") %>%
#'   ts_default() %>%
#'   ts_regular() %>%
#'   imputeTS::na_interpolation(option = "spline") %>%
#'   ts_span(start = 2005)
#' }
NULL
