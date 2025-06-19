
CalcCLfHf <- function(lf, hf, conversion, lf.end) {

  # # old start of period version
  HfPerLf <- function(lf, hf) {
    z <- integer(length(lf))

    for (i in 1:(length(lf) - 1)) {
      z[i] <- sum((hf >= lf[i]) & (hf < lf[i + 1]))
    }
    z[length(z)] <- sum(hf >= lf[length(lf)])
    z
  }

  # # end of period version
  # HfPerLf <- function(lf, hf){
  #   z <- integer(length(lf))

  #   z[1] <- sum(hf <= lf[1])
  #   for (i in 2:length(lf)){
  #     z[i] <- sum((hf > lf[i - 1]) & (hf <= lf[i]))
  #   }
  #   # z[length(z)] <- sum(hf >= lf[length(lf)])
  #   z
  # }

  # n.fc infered from the data
  # n.fc <- sum(hf > lf[length(lf)])
  n.fc <- sum(hf > lf.end)
  n.bc <- sum(hf < lf[1])

  hf.per.lf <- HfPerLf(lf, hf[hf <= lf.end & hf >= lf[1]])
  stopifnot(length(hf.per.lf) == length(lf))

  # set cfun according to type of conversion
  if (conversion == "sum") {
    cfun <- function(n) rep(1, n)
  } else if (conversion == "average") {
    cfun <- function(n) rep(1 / n, n)
  } else if (conversion == "first") {
    cfun <- function(n) c(1, rep(0, n - 1))
  } else if (conversion == "last") {
    cfun <- function(n) c(rep(0, n - 1), 1)
  } else {
    stop("Wrong type of conversion")
  }

  ll <- lapply(hf.per.lf, cfun)

  z <- list()
  for (i in 1:length(lf)) {
    l0 <- rep(list(0), length(lf))
    l0[[i]] <- ll[[i]]
    z[[i]] <- do.call(rbind, l0)
  }
  C <- do.call(cbind, z)

  if (n.fc > 0) {
    C <- cbind(C, matrix(0, nrow = nrow(C), ncol = n.fc))
  }
  if (n.bc > 0) {
    C <- cbind(matrix(0, nrow = nrow(C), ncol = n.bc), C)
  }
  stopifnot(nrow(C) == length(lf))
  stopifnot(ncol(C) == length(hf))

  C
}
