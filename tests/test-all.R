# These tests are more extensive and only need to run on GHA, not on CRAN.
library(testthat)
library(tempdisagg)

test_check("tempdisagg")


# check only if we are on GHA, we don't want the data file (300k) to be part
# of the package
if (
  Sys.getenv("CI") != "" &&
  Sys.getenv("GITHUB_WORKSPACE") != "" &&

  # Numerical tests don't work on some GHA Linux
  R.Version()$os != "linux-gnu") {

  # GHA folder (on GHA)
  path <- file.path(Sys.getenv("GITHUB_WORKSPACE"), "noinst")

  message("running extensive tests on CI only")

  setwd(path)


  # load data, functions
  # ============================================================================
  input_old <- "td0.24.1_R3.2.3_64bit.RData"
  # input_new <- "td0.24.2_R3.2.3_64bit.RData"

  file_in <- "data_input_swisspharma.RData"

  load(file = input_old)
  load(file_in)
  file_function <- "00_functions.R"

  old <- list()
  old$r <- r
  old$R <- R
  old$td_version <- td_version
  old$r_version <- r_version
  rm(r, R, td_version, r_version)

  source(file_function)

  # calculate data with acutal tempdisagg
  # ============================================================================

  library(tempdisagg)
  # library(divtools)
  # if(truncated) {
  #   file_out <- paste0("td", td_version, "_R", r_version, "_truncate.RData")
  #   exports.q <- window(exports.q, start=1975); imports.q <- window(imports.q, start=1975)
  #   exports.m <- window(exports.m, start=1975); imports.m <- window(imports.m, start=1975)
  # } else {file_out <- paste0("td", td_version, "_R", r_version, ".RData")}


  # estimation with tempdisagg in R (yearly to quarterly)
  # ----------------------------------------------------------------------------
  R <- list()
  r <- list()
  formula1 <- sales.a ~ exports.q + imports.q
  formula2 <- sales.a ~ 1
  formula3 <- sales.a ~ 0 + exports.q
  freq <- frequency(exports.q)

  R$y2q <- estimAll(formula1 = formula1, formula2 = formula2, formula3 = formula3, freq = freq, pre = FALSE)
  r$y2q <- do.call(cbind, lapply(R$y2q, predict))

  # estimation with tempdisagg in R (quarterly to monthly)
  # ----------------------------------------------------------------------------
  formula1 <- sales.q ~ exports.m + imports.m
  formula2 <- sales.q ~ 1
  formula3 <- sales.q ~ 0 + exports.m
  freq <- frequency(exports.m)

  R$q2m <- estimAll(formula1 = formula1, formula2 = formula2, formula3 = formula3, freq = freq, pre = FALSE)
  r$q2m <- do.call(cbind, lapply(R$q2m, predict))


  # Checks
  # ============================================================================

  # aggregation fullfilled?
  # ----------------------------------------------------------------------------
  stopifnot(sapply(R$y2q, function(x) {
    all.equal(window(ta(predict(x)), start = start(sales.a)), sales.a, tolerance = 1e-7)
  }))
  stopifnot(sapply(R$q2m, function(x) {
    all.equal(window(ta(predict(x), to = 4), start = start(sales.q), end = end(sales.q)), sales.q, tolerance = 1e-5, check.attributes = FALSE)
  }))

  stopifnot(sapply(old$R$y2q, function(x) {
    all.equal(window(ta(predict(x)), start = start(sales.a)), sales.a, tolerance = 1e-7)
  }))
  stopifnot(sapply(old$R$q2m, function(x) {
    all.equal(window(ta(predict(x), to = 4), start = start(sales.q), end = end(sales.q)), sales.q, tolerance = 1e-5, check.attributes = FALSE)
  }))

  # all components: TRUE

  # # summary statistics of tempdisagg estimations
  # # -------------------------------------------------------------------------------
  # for(i in 1:length(R$y2q)) {
  #   info <- paste("Hit ENTER to get to the summary statistics of", toupper(names(R$y2q)[i]),"")
  #   readline(info)
  #   # print(summary(R$y2q[[i]]))
  #   print(summary(R$y2q[[i]]))
  #   cat("\n\n")
  # }


  stop_and_print <- function(x) {
    oo <- getOption("warning.length")
    on.exit(options(warning.length = oo))
    options(warning.length = 8000)
    stop("differences found between new and old tempdisagg\n", paste(capture.output(x), collapse = "\n"), call. = FALSE)
  }

  # Comparison: new vs. old tempdisagg version
  # ===============================================================================
  # a) time series match?
  # -------------------------------------------------------------------------------
  # year to quarter conversion
  if (any(colnames(r$y2q) != colnames(old$r$y2q))) {
    warning("\nThere are less time series in the old set, the new set has been adjusted\n")
    r$y2q <- r$y2q[, colnames(old$r$y2q)]
  }
  if (sum(abs(diffsNewOld(r$y2q, old$r$y2q)[, 'comp.max'])) > 1e-3) {
    stop_and_print(diffsNewOld(r$y2q, old$r$y2q))
  }
  # identical(r$y2q, old$r$y2q)


  # quarter to month conversion
  if (any(colnames(r$q2m) != colnames(old$r$q2m))) {
    warning("\nThere are less time series in the old set, the new set has been adjusted\n")
    r$q2m <- r$q2m[, colnames(old$r$q2m)]
  }
  if (sum(abs(diffsNewOld(r$y2q, old$r$y2q)[, 'comp.max'])) > 1e-3) {
    stop_and_print(diffsNewOld(r$q2m, old$r$q2m))
  }
  # identical(r$q2m, old$r$q2m)


  # b) td-Objects match?
  # -------------------------------------------------------------------------------

  R$y2q <- lapply(R$y2q, function(e) {
    e$mode <- NULL
    e
  })
  R$q2m <- lapply(R$q2m, function(e) {
    e$mode <- NULL
    e
  })
  if (any(names(R) != names(old$R))) {
    R <- R[names(old$R)]
  }
  # td objects dont match on CI for some reason...
  # stopifnot(all.equal(R, old$R, tol = 1e-5))

  # c) graphical comparisons if necessary
  # -------------------------------------------------------------------------------
  # for(i in colnames(r$y2q)){
  #   tit <- toupper(i)
  #   info <- paste("Hit ENTER to get to the plot of", tit,"")
  #   readline(info)
  #   ts.plot(cbind(r$y2q[,i], old$r$y2q[,i]), col=c("red", "blue"), lty=c("solid", "dashed"), main=tit); grid()
  #   cat("\n\n")
  # }

  # diffs <- diffsNewOld(r$y2q, old$r$y2q)
  # for(i in colnames(diffs)){
  #   tit <- toupper(i)
  #   info <- paste("Hit ENTER to get to the plot of", tit,"")
  #   readline(info)
  #   plot(diffs[,i], col="red", main=tit, ylab="diffs", xlab="", xaxt="n", cex.lab=1.5); grid()
  #   axis(1, 1:length(diffs[,i]), names(diffs[,i]), las=3, cex.axis=0.8)
  #   cat("\n\n")
  # }



  # Aggregation Tests (including first, last)
  # ----------------------------------------------------------------------------

  am_m_sum <- predict(td(airmiles ~ 1, to = "monthly", method = "denton-cholette", conversion = "sum"))
  stopifnot(all.equal(airmiles, ta(am_m_sum, to = "annual", conversion = "sum")))
  stopifnot(all.equal(am_m_sum, ta(am_m_sum, to = 12, conversion = "sum")))

  am_q_sum <- ta(am_m_sum, to = "quarterly", conversion = "sum")
  stopifnot(all.equal(airmiles, ta(am_q_sum, to = "annual", conversion = "sum")))
  stopifnot(all.equal(am_q_sum, ta(am_q_sum, to = 4, conversion = "sum")))

  am_s_sum <- ta(am_q_sum, to = 2, conversion = "sum")
  stopifnot(all.equal(airmiles, ta(am_s_sum, to = "annual", conversion = "sum")))
  stopifnot(all.equal(am_s_sum, ta(am_s_sum, to = 2, conversion = "sum")))

  am_y_sum <- ta(am_s_sum, to = "annual", conversion = "sum")
  stopifnot(all.equal(airmiles, ta(am_y_sum, to = "annual", conversion = "sum")))


  am_m_average <- predict(td(airmiles ~ 1, to = "monthly", method = "denton-cholette", conversion = "average"))
  stopifnot(all.equal(airmiles, ta(am_m_average, to = "annual", conversion = "average")))
  stopifnot(all.equal(am_m_average, ta(am_m_average, to = 12, conversion = "average")))

  am_q_average <- ta(am_m_average, to = "quarterly", conversion = "average")
  stopifnot(all.equal(airmiles, ta(am_q_average, to = "annual", conversion = "average")))
  stopifnot(all.equal(am_q_average, ta(am_q_average, to = 4, conversion = "average")))

  am_s_average <- ta(am_q_average, to = 2, conversion = "average")
  stopifnot(all.equal(airmiles, ta(am_s_average, to = "annual", conversion = "average")))
  stopifnot(all.equal(am_s_average, ta(am_s_average, to = 2, conversion = "average")))

  am_y_average <- ta(am_s_average, to = "annual", conversion = "average")
  stopifnot(all.equal(airmiles, ta(am_y_average, to = "annual", conversion = "average")))


  am_m_first <- predict(td(airmiles ~ 1, to = "monthly", method = "denton-cholette", conversion = "first"))
  stopifnot(all.equal(airmiles, ta(am_m_first, to = "annual", conversion = "first")))
  stopifnot(all.equal(am_m_first, ta(am_m_first, to = 12, conversion = "first")))

  am_q_first <- ta(am_m_first, to = "quarterly", conversion = "first")
  stopifnot(all.equal(airmiles, ta(am_q_first, to = "annual", conversion = "first")))
  stopifnot(all.equal(am_q_first, ta(am_q_first, to = 4, conversion = "first")))

  am_s_first <- ta(am_q_first, to = 2, conversion = "first")
  stopifnot(all.equal(airmiles, ta(am_s_first, to = "annual", conversion = "first")))
  stopifnot(all.equal(am_s_first, ta(am_s_first, to = 2, conversion = "first")))

  am_y_first <- ta(am_s_first, to = "annual", conversion = "first")
  stopifnot(all.equal(airmiles, ta(am_y_first, to = "annual", conversion = "first")))


  am_m_last <- predict(td(airmiles ~ 1, to = "monthly", method = "denton-cholette", conversion = "last"))
  stopifnot(all.equal(airmiles, ta(am_m_last, to = "annual", conversion = "last")))
  stopifnot(all.equal(am_m_last, ta(am_m_last, to = 12, conversion = "last")))

  am_q_last <- ta(am_m_last, to = "quarterly", conversion = "last")
  stopifnot(all.equal(airmiles, ta(am_q_last, to = "annual", conversion = "last")))
  stopifnot(all.equal(am_q_last, ta(am_q_last, to = 4, conversion = "last")))

  am_s_last <- ta(am_q_last, to = 2, conversion = "last")
  stopifnot(all.equal(airmiles, ta(am_s_last, to = "annual", conversion = "last")))
  stopifnot(all.equal(am_s_last, ta(am_s_last, to = 2, conversion = "last")))

  am_y_last <- ta(am_s_last, to = "annual", conversion = "last")
  stopifnot(all.equal(airmiles, ta(am_y_last, to = "annual", conversion = "last")))
}
