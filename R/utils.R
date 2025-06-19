ModeOfSeries <- function(x) {
  tsobjs <- c(
    "zoo", "xts", "tslist", "tbl_ts", "timeSeries", "tbl_time", "tbl_df",
    "data.table", "data.frame", "dts", "tis", "irts"
  )

  if (inherits(x, "xts")) { # xts may be also a ts, treat as xts
    "tsbox"
  } else if (inherits(x, "ts")) {
    "ts"
  } else if (inherits(x, tsobjs)) {
    "tsbox"
  } else if (is.numeric(x)) {
    "numeric"
  } else {
    stop("series must be a time series object or numeric.", call. = FALSE)
  }
}
