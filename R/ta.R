#' Temporal Aggregation of Time Series
#'
#' Performs temporal aggregation of high to low frequency time series.
#' Currently, `ta` only works with `ts` or `mts` time series
#' objects.
#'
#' `ta` is used to aggregate a high frequency time series into a low
#' frequency series, while the latter is either the sum, the average, the first
#' or the last value of the high-frequency series. `ta` is the inverse
#' function of [td()]. If applied to an output series of `td`,
#' `ta` yields the original series.
#'
#' @param x           a time series object of class `"ts"` or `"mts"`.
#' @param conversion  type of conversion: `"sum"`, `"average"`,
#'                    `"first"` or `"last"`.
#' @param to          (low-frequency) destination frequency as a character
#'                    string (`"annual"` or `"quarterly"`) or as a
#'                    scalar (e.g. `1`, `2`, `4`).
#' @param ...         additional arguments, passed to the methods.
#'
#' @return `ta` returns an object of class `"ts"` or `"mts"`,
#'   depending on the class of the input series.
#'
#' @seealso [td()] for the main function for temporal disaggregation.
#' @export
#'
#' @examples
#' data(swisspharma)
#'
#' sales.q.a <- ta(sales.q, conversion = "sum", to = "annual")
#' all.equal(sales.a, sales.q.a)
#' @keywords ts models
ta <- function(x, ...) UseMethod("ta")


#' @rdname ta
#' @export
#' @import utils
#' @method ta ts
ta.ts <- function(x, conversion = "sum", to = "annual", ...) {
  if (ModeOfSeries(x) == "tsbox") {
    stop("ta() does not support ts-boxable time series. Use tsbox::ts_frequency() instead.")
  }

  # Calls SubAggregation for computation

  if (is.numeric(to)) { # frequency specified by a number
    f_l <- to
  } else if (is.character(to)) { # frequency specified by a char string
    if (to == "annual") {
      f_l <- 1
    } else if (to == "quarterly") {
      f_l <- 4
    } else {
      stop("unknown character string as the 'to' argument")
    }
  } else {
    stop("wrong specification of the 'to' argument")
  }

  if (!inherits(x, "ts")) stop("not a time series object.")

  if (inherits(x, "mts")) {
    ncol <- dim(x)[2]
    first <- SubAggregation(x[, 1], conversion = conversion, f_l = f_l)
    mat <- matrix(NA, nrow = length(first), ncol = ncol)
    mat[, 1] <- first # first column
    for (i in 2:ncol) { # remaining columns
      mat[, i] <- SubAggregation(x[, i], conversion = conversion, f_l = f_l)
    }
    z <- ts(mat, start = start(first), frequency = f_l)
    dimnames(z)[2] <- dimnames(x)[2]
  } else {
    z <- SubAggregation(x, conversion = conversion, f_l = f_l)
  }
  z
}


SubAggregation <- function(x, conversion = "sum", f_l = 1) {
  # performs a temporal agregation of a single time series
  #
  # Args:
  #   x:            a single time series object of class "ts"
  #   f_l:          frequency of the (low-frequency) destination series.
  #                 Overrides the to argument.
  #   to:           destination frequency ("quarterly" or "monthly"), only for
  #                 Denton without indicators
  #
  # Returns:
  #   A time series object of class "ts"

  f <- frequency(x)
  fr <- f / f_l

  hf.start <- time(x)[!is.na(x)][1]
  hf.start.na <- time(x)[1]
  hf.end <- tail(time(x)[!is.na(x)], 1)
  hf.end.na <- c(tail(time(x), 1))

  lf.start <- SubConvertStart(hf.start = hf.start, f = f, f_l = f_l)
  lf.start.na <- SubConvertStart(hf.start = hf.start.na, f = f, f_l = f_l)

  lf.end <- SubConvertEnd(hf.end = hf.end, f = f, f_l = f_l)
  lf.end.na <- SubConvertEnd(hf.end = hf.end.na, f = f, f_l = f_l)

  # first and last are calculated without using the C matrix
  # a low frequency value can also be calculated if the high frequency data is
  # incomplete (#22)
  if (conversion == "first") {
    if (f == 12) {
      if (f_l == 12) cc <- 1:12 # including trivial f = f_l case
      if (f_l == 4) cc <- c(1, 4, 7, 10)
      if (f_l == 2) cc <- c(1, 7)
      if (f_l == 1) cc <- c(1)
    }
    if (f == 4) {
      if (f_l == 4) cc <- 1:4
      if (f_l == 2) cc <- c(1, 3)
      if (f_l == 1) cc <- c(1)
    }
    if (f == 2) {
      if (f_l == 2) cc <- 1:2
      if (f_l == 1) cc <- c(1)
    }
    if (f == 1) {
      if (f_l == 1) cc <- c(1)
    }
    iscc <- cycle(x) %in% cc
    # lf starting period is calculate correctly with SubConvertStart
    lf.start <- SubConvertStart(hf.start = time(x)[iscc][1], f = f, f_l = f_l)
    z <- ts(x[iscc], start = lf.start, frequency = f_l)
    return(z)
  }
  if (conversion == "last") {
    if (f == 12) {
      if (f_l == 12) cc <- 1:12
      if (f_l == 4) cc <- c(3, 6, 9, 12)
      if (f_l == 2) cc <- c(6, 12)
      if (f_l == 1) cc <- c(12)
    }
    if (f == 4) {
      if (f_l == 4) cc <- 1:4
      if (f_l == 2) cc <- c(2, 4)
      if (f_l == 1) cc <- c(4)
    }
    if (f == 2) {
      if (f_l == 2) cc <- 1:2
      if (f_l == 1) cc <- c(2)
    }
    if (f == 1) {
      if (f_l == 1) cc <- c(1)
    }
    iscc <- cycle(x) %in% cc

    lf.start <- SubConvertStart(hf.start = time(x)[iscc][1], f = f, f_l = f_l)
    # lf starting period is calculated incorrectly by SubConvertStart, since it
    # assumes hf period is not complete. Shifting by 1 lf period.
    if (f_l != f) {
      lf.start <- lf.start - 1 / f_l
    }
    z <- ts(x[iscc], start = lf.start, frequency = f_l)
    return(z)
  }

  # if all observations are NAs, return NAs
  if (all(is.na(x))) {
    z <- window(ts(NA, start = lf.start.na, frequency = f_l),
      end = lf.end.na, extend = TRUE
    )
    # if series contains insufficient numbers of observations, return NAs
  } else if (lf.start > lf.end) {
    z <- window(ts(NA, start = lf.start.na, frequency = f_l),
      end = lf.end.na, extend = TRUE
    )
  } else {
    x.used <- window(x, start = lf.start, end = lf.end + 1 / f_l - 1 / f)
    if (any(is.na(x.used))) {
      warning("time series contains internal NAs")
    }
    agg <- as.numeric(CalcC(
      n_l = length(x.used) / fr, conversion = conversion,
      fr = fr
    ) %*% x.used)
    agg.ts <- ts(agg, start = lf.start, frequency = f_l)
    z <- window(agg.ts, start = lf.start.na, end = lf.end.na, extend = TRUE)
  }
  z
}


SubConvertEnd <- function(hf.end, f, f_l) {
  # converts a hf end point to the last fully available lf end point
  #
  # Args:
  #   hf.end:       a scalar indicating the time stamp of the last available
  #                   high frequency obs.
  #   f:            frequency of the high-frequency series
  #   f_l:          frequency of the low-frequency series
  #
  # Returns:
  #   a scalar indicating the time stamp of last complete low frequency value
  #
  # Remarks:
  #   Identical to SubConvertStart() except that ceiling() is exchanged by
  #   floor()

  fr <- f / f_l
  floor(hf.end) + (floor(
    ((hf.end - floor(hf.end)) * f + 1 + 1e-8) / fr
  ) - 1) / f_l
  # +1e-8 avoids rounding problems
}


SubConvertStart <- function(hf.start, f, f_l) {
  # converts a hf end point to the last fully available lf end point
  #
  # Args:
  #   hf.start:     a scalar indicating the time stamp of the first available
  #                   high frequency series
  #   f:            frequency of the high-frequency series
  #   f_l:          frequency of the low-frequency series
  #
  # Returns:
  #   a scalar indicating the time stamp of last complete low frequency value
  #
  # Remarks:
  #   Identical to SubConvertEnd() except that floor() is exchanged by ceiling().

  fr <- f / f_l
  floor(hf.start) + ceiling(((hf.start - floor(hf.start)) * f) / fr - 1e-8) / f_l
  # -1e-8 avoids rounding problems
}
