#' Temporal Disaggregation of Time Series
#'
#' Perform temporal disaggregation or interpolation of low frequency to high
#' frequency time series. `td` can be used with objects of class
#' `"ts"`, with numeric vectors or with any
#' [ts-boxable](https://docs.ropensci.org/tsbox/) time series object.
#'
#' `td` is used to disaggregate or interpolate a low frequency to a higher
#' frequency time series, while either the sum, the average, the first or the
#' last value of the resulting high-frequency series is consistent with the low
#' frequency series. Disaggregation can be performed with or without the help of
#' one or more right hand side indicator series. It can deal with both with
#' a regular disaggregation setting (e.g. quarters to months) but also with
#' an irregular disaggregation setting (e.g. months to days), where it respects
#' the the different lengths of the months.
#'
#' If the high-frequency indicator(s) cover(s) a longer time span than the
#' low-frequency series, an extrapolation or retropolation (Wei, 1994, p. 138)
#' is performed, using the same model as for interpolation.
#'
#' The selection of a temporal disaggregation model is similar to the selection
#' of a linear regression model. Thus, `td` closely mirrors the working of
#' the [lm()] function. The left hand side of the
#' [formula()] denotes the low-frequency series, the right hand side
#' the indicators. If no indicator is specified, the right hand side must be set
#' equal to `1` (see examples). Unlike `lm`, `td` handles
#' [ts()] and `mts` time-series objects, as a typical application
#' involves the use of these objects. Alternatively, If used with basic vectors,
#' the `to` argument specifies the ratio between the high and the low
#' frequency series.
#'
#' For the generalized least squares (GLS) methods `"chow-lin-maxlog"`,
#' `"chow-lin-minrss-ecotrim"`, `"chow-lin-minrss-quilis"`,
#' `"litterman-maxlog"` and `"litterman-minrss"`, an autoregressive
#' parameter \eqn{\rho} is estimated. Default (and recommended) method is
#' `chow-lin-maxlog`. With `truncated.rho = 0` (default), it produces
#' good results for a wide range of applications.
#'
#' There are two variants of the `chow-lin-minrss` approach that lead to
#' different results: Ecotrim by Barcellan (2003) uses a correlation matrix
#' instead of the variance covariance matrix (implemented in
#' `"chow-lin-minrss-ecotrim"`), the Matlab library by Quilis (2009)
#' multiplies the correlation matrix with \eqn{1/(1-\rho^2)} (implemented in
#' `"chow-lin-minrss-quilis"`).
#'
#' The methods `"dynamic-maxlog"`, `"dynamic-minrss"` and
#' `"dynamic-fixed"` are dynamic extensions of Chow-Lin (Santos Silva and
#' Cardoso, 2001). If the autoregressive parameter \eqn{\rho} is equal to 0, no
#' truncation remainder is added.
#'
#' The Denton methods `"denton"` and `"denton-cholette"` can be
#' specified with one or without an indicator. The parameter `h` can be set
#' equal to `0`, `1`, or `2`. Depending on the value, the
#' `denton` procedure minimizes the sum of squares of the deviations
#' between the levels (`0`), the first differences (`1`) or the second
#' differences (`2`) of the indicator and the resulting series.
#' Additionally, `criterion` can be set equal to `"proportional"` or
#' `"additive"`, depending on whether the proportional or the absolute
#' deviations should be considered for minimzation. `"denton-cholette"`
#' removes the transient movement of the original `"denton"` method at the
#' beginning of the resulting series.  `"fast"` is a shortcut for
#' `"chow-lin-fixed"` with `fixed.rho = 0.99999`. It returns approximately the
#' same results as "denton-cholette" with `h = 1`, but is much faster.
#'
#' `"uniform"` is a special case of the `"denton"` approach, with
#' `h` equals  `0` and `criterion` equals  `"additive"`.
#' It distributes the residuals uniformly. If no indicator is used, this leads
#' to a step-shaped series.
#'
#' `"ols"` performs an ordinary least squares regression (OLS) and
#' distributes the residuals uniformly. It is especially useful for comparing
#' the estimators of GLS and OLS regressions.
#'
#' @param formula     an object of class `"formula"`: a symbolic
#'   description of the the temporal disaggregation model. The details of model
#'   specification are given under 'Details'.
#' @param conversion  type of conversion: `"sum"`, `"mean"` (or: `"average"`),
#'   `"first"` or `"last"`.
#' @param method      method of temporal disaggregation:
#'   `"chow-lin-maxlog"`, `"chow-lin-minrss-ecotrim"`,
#'   `"chow-lin-minrss-quilis"`, `"chow-lin-fixed"`,
#'   `"dynamic-maxlog"` (experimental), `"dynamic-minrss"` (experimental), `"dynamic-fixed"` (experimental),
#'   `"fernandez"`, `"litterman-maxlog"`, `"litterman-minrss"`,
#'   `"litterman-fixed"`, `"denton-cholette"`, `"denton"`, `"fast"`,
#'   `"uniform"` or `"ols"`. See 'Details'.
#' @param to          high-frequency destination frequency as a character string
#'   (`"quarter"` (or `"quarterly"`), `"month"` (or `"monthly"`), `"day"`,
#'   `"hour"`, `"minute"`, `"second"`, or `"year"`)
#'   or as a scalar (e.g. `2`, `4`, `7`, `12`). Required if no right hand side
#'   indicator series is provided. The [tsbox](https://docs.ropensci.org/tsbox/) package must
#'   be installed to deal with frequencies other than monthly or quarterly. If
#'   the input series are numeric, `to` is a scalar indicating the
#'   frequency ratio.
#' @param truncated.rho  lower bound for the autoregressive parameter
#'   \eqn{\rho}. If set to `0` (default), no negative values are allowed.
#'   If set to `-1`, truncation is disabled.
#' @param fixed.rho   set a predefined autoregressive parameter \eqn{\rho}. Only
#'   works with the methods `"chow-lin-fixed"` and
#'   `"litterman-fixed"`.
#' @param criterion   minimzation criterion for Denton methods:
#'   `"proportional"` or `"additive"`.  See 'Details'.
#' @param h           degree of differencing for Denton methods. See 'Details'.
#' @param start       (optional) start date. Similar to pre-processing the input
#'   series with [window()].
#' @param end         (optional) end date. Similar to pre-processing the input
#'   series with [window()].
#' @param ...         additional arguments to be passed to the low level
#'   subfunctions.
#' @return `td` returns an object of class `"td"`.
#'
#'   The function [`predict()`][predict.td] computes the interpolated
#'   high frequency series. If the high-frequency indicator series are longer
#'   than the low-frequency series, the resulting series will be extrapolated.
#'   The function `coefficients` extracts the coefficients. The function
#'   `residuals` extracts the low frequency residuals. The function
#'   [`summary()`][summary.td] prints a summary of the estimation.
#'
#'   An object of class `"td"` is a list containing the following
#'   components: \item{values}{disaggregated or interpolated (and extrapolated)
#'   high frequency series} \item{fitted.values}{low frequency fitted values of
#'   the regression; low frequency indicator for the Denton methods.}
#'   \item{p}{preliminary high frequency series} \item{residuals}{low-frequency
#'   residuals} \item{rho}{autoregressive parameter, \eqn{\rho}}
#'   \item{truncated}{logical, whether \eqn{\rho} has been truncated}
#'   \item{coefficients}{a named vector of coefficients} \item{se}{standard
#'   errors of the coefficients} \item{s_2}{ML-estimator of the variance of the
#'   high-frequency residuals} \item{s_2_gls}{GLS-estimator of the variance of
#'   the high-frequency residuals} \item{tss}{weighted (low frequency) total sum
#'   of squares} \item{rss}{weighted (low frequency) residual sum of squares}
#'   \item{r.squared}{R squared} \item{adj.r.squared}{adjusted R squared}
#'   \item{logl}{log-likelihood} \item{aic}{Akaike information criterion}
#'   \item{bic}{Schwarz information criterion} \item{rank}{number of right hand
#'   variables (including intercept)} \item{df}{degrees of freedom}
#'   \item{method}{method of temporal disaggregation} \item{call}{function call}
#'   \item{name}{name of the low frequency variable} \item{fr}{the ratio of high
#'   to low-frequency series} \item{conversion}{type of temporal conversion}
#'   \item{actual}{actual values of the low frequeny series} \item{model}{a
#'   matrix containing the indicators (and a constant if present)}
#'   \item{criterion}{minimization criterion in Denton methods} \item{h}{order
#'   of differencing in Denton methods}
#'
#' @references  Chow, G. C., & Lin, A. L. (1971). Best linear unbiased
#'   interpolation, distribution, and extrapolation of time series by related
#'   series. *The review of Economics and Statistics*, 372-375.
#'
#'   Denton, F. T. (1971). Adjustment of monthly or quarterly series to annual
#'   totals: an approach based on quadratic minimization. *Journal of the
#'   American Statistical Association*, 66(333), 99-102.
#'
#'   Santos Silva, J. M. C. & Cardoso, F. N. (2001). The Chow-Lin method using
#'   dynamic models. *Economomic Modelling*, 18, 269-280.
#'
#'   Wei, W. W. S. (1994). Time series analysis. Addison-Wesley publ.
#'
#'   Sax, C. und Steiner, P. (2013). Temporal Disaggregation of Time Series.
#'   *The R Journal*, 5(2), 80-88. \doi{10.32614/RJ-2013-028}
#'
#' @seealso [ta()] for temporal aggregation, the inverse function of
#'   `td`.
#'
#'   [`summary()`][summary.td] is used to obtain and print a summary of
#'   the results.
#'
#'   [`predict()`][predict.td] is used to extract the disaggregated or
#'   interpolated high frequency series.
#'
#'   [`plot()`][plot.td] is used to plot the fitted and actual low
#'   frequency series, as well as the residuals.
#'
#' @examples
#' data(tempdisagg)
#'
#' # one indicator, no intercept
#' mod1 <- td(sales.a ~ 0 + exports.q)
#' summary(mod1)  # summary statistics
#' plot(mod1)  # residual plot of regression
#' plot(predict(mod1))
#'
#' # interpolated quarterly series
#'
#' # temporally aggregated series is equal to the annual value
#' all.equal(window(
#'   ta(predict(mod1), conversion = "sum", to = "annual"),
#'   start = 1975), sales.a)
#'
#' # several indicators, including an intercept
#' mod2 <- td(sales.a ~ imports.q + exports.q)
#'
#' # no indicator (Denton-Cholette)
#' mod3 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")
#'
#' # no indicator (uniform)
#' mod4 <- td(sales.a ~ 1, to = "quarterly", method = "uniform")
#'
#' # Dynamic Chow-Lin (Santos Silva and Cardoso, 2001)
#' # (no truncation parameter added, because rho = 0)
#' mod5 <- td(sales.a ~ exports.q, method = "dynamic-maxlog")
#'
#' # Example from Denton (1971), see references.
#' d.q <- ts(rep(c(50, 100, 150, 100), 5), frequency = 4)
#' d.a <- ts(c(500, 400, 300, 400, 500))
#'
#' a1 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "additive", h = 0))
#' a2 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "additive", h = 1))
#' a3 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "additive", h = 2))
#' a4 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "additive", h = 3))
#'
#' p1 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "proportional", h = 0))
#' p2 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "proportional", h = 1))
#' p3 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "proportional", h = 2))
#' p4 <- predict(td(d.a ~ 0 + d.q, method = "denton",
#'                  criterion = "proportional", h = 3))
#'
#' # Table in Denton (1971), page 101:
#' round(cbind(d.q, a1, a2, a3, a4, p1, p2, p3, p4))
#'
#' \dontrun{
#'
#' # Using altvernative time series classes (see https://docs.ropensci.org/tsbox/)
#' library(tsbox)
#' sales.a.xts <- ts_xts(window(sales.a, start = 2000))
#' exports.q.xts <- ts_xts(window(exports.q, start = 2000))
#' mod1b <- td(sales.a.xts ~ 0 + exports.q.xts)
#' predict(mod1b)  # class 'xts'
#'
#' # non-standard frequencies: decades to years
#' predict(td(ts_xts(uspop) ~ 1, "mean", to = "year", method = "fast"))
#'
#' # quarter to daily (no indicator)
#' m.d.noind <- td(gdp.q ~ 1, to = "daily", method = "fast")
#' predict(m.d.noind)
#'
#' # quarter to daily (one indicator)
#' m.d.stocks <- td(gdp.q ~ spi.d, method = "chow-lin-fixed", fixed.rho = 0.9)
#' predict(m.d.stocks)
#' }
#' @keywords ts models
#' @export
#'
td <- function(formula, conversion = "sum", to = "quarterly",
               method = "chow-lin-maxlog", truncated.rho = 0, fixed.rho = 0.5,
               criterion = "proportional", h = 1,
               start = NULL, end = NULL, ...) {

  # td deals with the formula interface, the time-series properties
  # and allows for optional shortening of the series. The estimation itself is
  # done by subfunctions starting with Sub...

  cl <- match.call()

  if (conversion == "mean") conversion <- "average" # equivalent

  if (method == "fast") {
    method <- "chow-lin-fixed"
    fixed.rho <- 0.99999
  }

  # --- input consistency ------------------------------------------------------

  # dont allow length = 2 vectors as start or end inputs
  if (any(c(length(start), length(end)) > 1)) {
    stop("'start' or 'end' must of length 1")
  }

  if (method == "denton") {
    message(
      "'denton-cholette' removes the transient movement at the beginning of ",
      "the series and is preferable to the original 'denton' method in most ",
      "cases."
    )
  }

  # ---- prepare Formula, extract names and data -------------------------------

  # extract X (right hand side, high frequency) formula, names and data
  X.formula <- formula
  X.formula[[2]] <- NULL
  X.series.names <- all.vars(X.formula)

  # extract y_l (left hand side, low frequency) formula, values and names
  y_l.formula <- formula[[2]]
  y_l.series <- na.omit(eval(y_l.formula, envir = environment(formula)))
  y_l.name <- deparse(y_l.formula)

  # --- determine tsmode -------------------------------------------------------

  # 3 modes: "tsbox", "ts", "numeric"
  mode <- ModeOfSeries(y_l.series)

  # mixed cases
  # LHS ts, RHS tsbox -> tsbox
  if (
    mode == "ts" &&
      length(X.series.names) > 0 &&
      ModeOfSeries(get(X.series.names[1], envir = environment(X.formula))) == "tsbox"
  ) {
    mode <- "tsbox"
  }
  # LHS ts, RHS numeric -> numeric
  if (
    mode == "ts" &&
      length(X.series.names) > 0 &&
      ModeOfSeries(get(X.series.names[1], envir = environment(X.formula))) == "numeric"
  ) {
    warning("Only left hand side is a time series. Using numeric mode.")
    mode <- "numeric"
  }

  if (mode == "ts" && !(to %in% list("quarterly", "monthly", "quarter", "month", 2, 4, 7, 12))) {
    stop("use a time series class other than 'ts' to deal with '", to, "'")
  }

  # ---- tsbox mode ------------------------------------------------------------

  if (mode == "tsbox") {
    fr <- NULL

    if (!requireNamespace("tsbox")) {
      stop(
        "'tsbox' is needed to support non-standard time-series class. To ",
        "install: \n\n  install.packages(\"tsbox\")"
      )
    }
    lf.dt <- tsbox::ts_regular(tsbox::ts_default(tsbox::ts_dts(y_l.series)))
    if (any(is.na(lf.dt$value))) stop("left hand side contains NAs")
    if (ncol(lf.dt) > 2) {
      stop("left hand side must not contain more than one series")
    }
    lf <- lf.dt$time

    y_l <- as.matrix(lf.dt$value)

    if (length(X.series.names) > 0) {
      X.modes <- vapply(X.series.names, function(e) ModeOfSeries(get(e, envir = environment(X.formula))), "")
      is.time.series <- X.modes %in% c("ts", "tsbox")
      if (!all(is.time.series)) {
        stop("some right hand side variables are not valid time series: ", X.modes[!is.time.series])
      }

      X.objects <- setNames(
        lapply(X.series.names, function(e) get(e, envir = environment(X.formula))),
        X.series.names
      )
      X.template <- X.objects[[1]]
      X.dtss <- lapply(
        X.objects,
        function(e) tsbox::ts_regular(tsbox::ts_default(tsbox::ts_dts(e)))
      )
      smry <- tsbox::ts_summary(do.call(tsbox::ts_c, X.dtss))
      if (length(unique(smry$start)) > 1) stop("non-unique start on RHS: ", paste(smry$start, collapse = ", "))
      if (length(unique(smry$end)) > 1) stop("non-unique end on RHS: ", paste(smry$end, collapse = ", "))
      if (length(unique(smry$diff)) > 1) stop("non-unique frequency on RHS: ", paste(smry$diff, collapse = ", "))
      to <- unique(smry$diff)

      X.wide.dfs <- lapply(X.dtss, function(e) tsbox::ts_wide(tsbox::ts_dt(e)))
      X.matrices <- lapply(X.wide.dfs, function(e) as.matrix(e[, -1]))
      if (any(is.na(do.call(cbind, X.matrices)))) stop("right hand side contains NAs")
      X.times <- lapply(X.wide.dfs, function(e) e$time)

      hf <- X.times[[1]]
      stopifnot(all(vapply(X.times, identical, TRUE, hf)))

      if (lf[1] < hf[1]) {
        lf.dt <- tsbox::ts_span(lf.dt, start = hf[1])
        y_l.series <- tsbox::ts_span(y_l.series, start = hf[1])
        lf <- lf[lf >= hf[1]]
        message("High frequency series shorter than low frequency. Discarding low frequency before ", lf[1], ".")
      }

      # last time stamp covered by lf, in hf units. This could be infered from hf
      # and lf only.
      hf.by.string <- paste0("-", if (!grepl("^\\d", to)) "1 ", to)
      lf.end <- tail(tsbox::ts_lag(tsbox::ts_bind(lf.dt, NA), by = hf.by.string)$time, 1)

      # data matrices
      hf.env <- list2env(as.list(X.matrices))
      hf.dt.formula <- X.formula

      attr(hf.dt.formula, ".Environment") <- hf.env
      X <- model.matrix(hf.dt.formula)
      X.names <- dimnames(X)[[2]]
    } else {
      # if there is no X Variables, set it to a constant ('Denton' Methods)
      to <- gsub("daily$", "day", to)
      to <- gsub("ly$", "", to)
      hf.by.string <- paste0("-", if (!grepl("^\\d", to)) "1 ", to)
      lf.end <- tail(tsbox::ts_lag(tsbox::ts_bind(lf.dt, NA), by = hf.by.string)$time, 1)
      hf.dt <- tsbox::ts_dts(data.frame(time = seq(lf[1], lf.end, by = to), value = 1))
      X.template <- tsbox::copy_class(hf.dt, y_l.series)
      X <- as.matrix(hf.dt$value)
      X.names <- "(Intercept)"
      hf <- hf.dt$time
    }


    # final data matrices
    y_l <- as.matrix(lf.dt$value)
    X <- matrix(X, nrow = nrow(X), ncol = ncol(X))
    dimnames(X) <- list(NULL, X.names)
    stopifnot(identical(dim(y_l)[1], length(lf)))
    stopifnot(identical(dim(X)[1], length(hf)))
  }

  # ---- ts mode ---------------------------------------------------------------

  if (mode == "ts") {

    ### y_l.series
    # shorten y_l.series if start or end are specified
    y_l.series <- window(y_l.series, start = start, end = end)
    f_l <- frequency(y_l.series)
    y_l.time <- time(y_l.series)[!is.na(y_l.series)]
    start <- y_l.time[1]
    end <- y_l.time[length(y_l.time)]

    ### X.series
    if (length(X.series.names) > 0) { # If X is pecified
      # prototype series of X
      X.series.proto <- eval(X.formula[[2]], envir = environment(X.formula))
      X.start <- time(na.omit(X.series.proto))[1]
      X.end <- time(na.omit(X.series.proto))[length(time(na.omit(X.series.proto)))]

      f <- frequency(X.series.proto)
      fr <- as.integer(round(f / f_l))


      # determine first and last fully available lf time stamps
      X.start_l <- SubConvertStart(hf.start = X.start, f = f, f_l = f_l)
      X.end_l <- SubConvertEnd(hf.end = X.end, f = f, f_l = f_l)
      if (X.start_l > start + 0.001) {
        start <- X.start_l
        message("High frequency series shorter than low frequency. Discarding low frequency before ", start, ".")
        y_l.series <- window(y_l.series, start = start)
      }
      if (X.end_l < end - 0.001) {
        end <- X.end_l
        message("High frequency series shorter than low frequency. Discarding low frequency after ", end, ".")
        y_l.series <- window(y_l.series, end = end)
      }

      # number of high frequency periods for backcast/forecast
      n.bc <- as.integer(round((start - X.start) * f))
      n.fc <- as.integer(round((X.end - end) * f)) - fr + 1L
    } else { # If no X is specified
      if (is.numeric(to)) { # frequency specified by a number
        f <- to
      } else if (is.character(to)) { # frequency specified by a char string
        if (to %in% c("quarterly", "quarter")) {
          f <- 4L
        } else if (to %in% c("monthly", "month")) {
          f <- 12L
        } else {
          stop("'to' argument: unknown character string")
        }
      } else {
        stop("'to' argument: wrong specification")
      }
      stopifnot(f_l <= f)
      fr <- as.integer(round(f / f_l))
      X.start <- start
      n.bc <- 0L
      n.fc <- 0L
    }
  }

  # ---- numeric mode ----------------------------------------------------------

  if (mode == "numeric") {
    if (!is.numeric(to)) {
      stop("In numeric mode, 'to' must be an integer number.")
    }
    f_l <- 1L
    f <- to
    fr <- as.integer(round(f / f_l))
    n.bc <- 0L
    if (length(X.series.names) > 0) {
      n.fc <- length(get(X.series.names[1], envir = environment(X.formula))) -
        fr * length(y_l.series)
    } else {
      n.fc <- 0L
    }
  }

  # --- final data matrices (ts, numeric) --------------------------------------

  if (mode %in% c("ts", "numeric")) {
    hf <- lf <- lf.end <- NULL

    if (length(X.series.names) > 0) {
      X <- model.matrix(X.formula)
      X.names <- dimnames(X)[[2]]
    } else {
      # if there is no X Variables, set it to a constant ('Denton' Methods)
      if (is.null(hf)) {
        X <- matrix(rep(1, times = length(y_l.series) * fr))
      } else {
        X <- matrix(rep(1, length(hf)))
      }
      X.names <- "(Intercept)"
    }

    # final data matrices
    y_l <- as.matrix(y_l.series)
    X <- matrix(X, nrow = nrow(X), ncol = ncol(X))
    dimnames(X) <- list(NULL, X.names)
  }

  # --- estimation -------------------------------------------------------------

  # actual estimation
  if (method %in% c(
    "chow-lin-maxlog", "chow-lin-minrss-ecotrim",
    "chow-lin-minrss-quilis", "chow-lin-fixed",
    "litterman-maxlog", "litterman-minrss", "litterman-fixed",
    "fernandez", "dynamic-maxlog", "dynamic-minrss",
    "dynamic-fixed", "ols"
  )) {
    z <- SubRegressionBased(
      y_l = y_l, X = X, hf = hf, lf = lf, lf.end = lf.end,
      n.bc = n.bc, n.fc = n.fc,
      conversion = conversion,
      method = method, truncated.rho = truncated.rho,
      fixed.rho = fixed.rho, fr = fr, ...
    )
  } else if (method %in% c("denton-cholette", "denton", "uniform")) {
    z <- SubDenton(
      y_l = y_l, X = X, hf = hf, lf = lf, lf.end = lf.end,
      n.bc = n.bc, n.fc = n.fc,
      conversion = conversion, method = method,
      criterion = criterion, h = h, fr = fr, ...
    )
  } else {
    stop("method does not exist")
  }


  # --- output ----------------------------------------------------------------

  # add coefficent names to output
  if (!is.null(z$coefficients)) {
    if (method %in% c("dynamic-maxlog", "dynamic-minrss", "dynamic-fixed")) {
      # add name for trunc. remainder if present
      if (length(z$coefficients) == (length(X.names) + 1)) {
        X.names <- c(X.names, "tr. rem.")
      }
    }
    names(z$coefficients) <- names(z$se) <- X.names[1:length(z$coefficients)]
  }

  # additional output
  z$mode <- mode
  z$method <- method
  z$call <- cl
  z$name <- y_l.name
  z$fr <- fr
  z$conversion <- conversion
  z$actual <- y_l.series
  z$model <- X
  if (mode == "ts") {
    z$model <- ts(z$model, start = X.start, frequency = f)
    z$p <- ts(z$p, start = X.start, frequency = f)
    z$values <- ts(z$values, start = X.start, frequency = f)
    z$fitted.values <- ts(z$fitted.values, start = start, frequency = f_l)
    z$residuals <- ts(z$residuals, start = start, frequency = f_l)
    z$actual <- ts(z$actual, start = start, frequency = f_l)
  } else if (mode == "tsbox") {
    z$model <- tsbox::copy_class(z$model, X.template)
    z$p <- tsbox::copy_class(z$p, X.template)
    z$values <- tsbox::copy_class(z$values, X.template)
    z$fitted.values <- tsbox::copy_class(z$fitted.values, y_l.series)
    z$residuals <- tsbox::copy_class(z$residuals, y_l.series)
    z$actual <- tsbox::copy_class(z$actual, y_l.series)
  }
  class(z) <- "td"
  z
}
