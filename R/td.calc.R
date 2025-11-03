CalcC <- function(n_l, conversion, fr, n.bc = 0, n.fc = 0) {
  # calculates the conversion matrix C, optionally expanded with zeros
  #
  # Args:
  #   n_l:          number of low-frequency observations
  #   conversion:   char string indicating the type of conversion
  #                 ("sum", "average", "first", "last")
  #   fr:           ratio of high-frequency units per low-frequency unit
  #   n:            number of high-frequency observations
  #                 (matrix will be expanded by 0 if n is provided and > n_l*fr)
  #
  # Returns:
  #   conversion matrix

  # sanity checks
  stopifnot(n.bc >= 0)
  stopifnot(n.fc >= 0)

  # set conversion.weights according to type of conversion
  if (conversion == "sum") {
    conversion.weights <- rep(1, fr)
  } else if (conversion == "average") {
    conversion.weights <- rep(1, fr) / fr
  } else if (conversion == "first") {
    conversion.weights <- numeric(fr)
    conversion.weights[1] <- 1
  } else if (conversion == "last") {
    conversion.weights <- numeric(fr)
    conversion.weights[fr] <- 1
  } else {
    stop("Wrong type of conversion")
  }

  # compute the conversion matrix
  C <- kronecker(diag(n_l), t(conversion.weights))
  if (n.fc > 0) {
    C <- cbind(C, matrix(0, nrow = n_l, ncol = n.fc))
  }
  if (n.bc > 0) {
    C <- cbind(matrix(0, nrow = n_l, ncol = n.bc), C)
  }
  C
}


CalcPowerMatrix <- function(n) {
  # calculates a symetric 'power' matrix with 0 on the diagonal,
  #   1 in the subsequent diagonal, and so on.
  #
  # Args:
  #   n:           number of high-frequency observations
  #
  # Returns:
  #   power matrix

  mat <- diag(n)
  abs(row(mat) - col(mat))
}


CalcR <- function(rho, pm) {
  # calculates a correlation matrix R
  #
  # Args:
  #   rho:        autoregressive parameter
  #   pm:         power matrix, as calculated by CalcPowerMatrix()
  #
  # Returns:
  #   correlation matrix
  rho^pm
}


CalcQ <- function(rho, pm) {
  # calculates the s_2-factored-out vcov matrix Q
  #
  # Args:
  #   rho:        autoregressive parameter
  #   pm:         power matrix, as calculated by CalcPowerMatrix()
  #
  # Returns:
  #   s_2-factored-out vcov matrix Q
  (1 / (1 - rho^2)) * CalcR(rho, pm)
}


CalcQ_Lit <- function(X, rho = 0) {
  # calculates the (pseudo) vcov matrix for
  #   a Random Walk (RW) (with opt. AR1)
  #
  # Args:
  #   X:            matrix of high-frequency indicators
  #   rho:          if != 0, a AR1 is added to the RW (Litterman)
  #
  # Returns:
  #   pseudo vcov matrix

  # dimension of y_l
  n <- dim(X)[1]

  # calclation of S
  H <- D <- diag(n)
  diag(D[2:nrow(D), 1:(ncol(D) - 1)]) <- -1
  diag(H[2:nrow(H), 1:(ncol(H) - 1)]) <- -rho
  Q_Lit <- solve(t(D) %*% t(H) %*% H %*% D)

  # output
  Q_Lit
}


CalcGLS <- function(y, X, vcov, logl = TRUE, stats = TRUE) {
  # computationally efficient and numerically stable GLS estimation
  #
  # Args:
  #   y:            vector with LHS data
  #   X:            matrix with RHS data (same frequency as y)
  #   vcov:         (pseudo) variance covariance matrix
  #   logl:         logical, compute logl of the regression
  #   se:           logical, compute standard errors of the regression
  #
  # Returns:
  #   A list containing the following elements:
  #   coefficients  vector, GLS coefficients
  #   rss           scalar, generalized residual sum of square
  #   tss           scalar, generalized total sum of square
  #   logl          scalar, log-likelihood
  #   s_2:          scalar, ML-estimator of the variance of the regression
  #   s_2_gls:      scalar, GLS-estimator of the variance of the regression
  #   se            vector, standard errors of the coefficients
  #   rank          scalar, number of right hand variables (including intercept)
  #   df            scalar, degrees of freedom
  #   vcov_inv      matrix, inverted vcov matrix
  #   r.squared     scalar, R2
  #   adj.r.squared scalar, adj. R2
  #   aic           scalar, Akaike information criterion
  #   bic           scalar, Bayesian information criterion
  #
  # Remarks:
  #   Algorithm developed by Paige, 1979. The implementation is based on:
  #   Ake Bjoerck, 1990: Numerical Methods for Least Squares Problems, S. 164
  #   http://books.google.ch/books?id=ZecsDBMz5-IC&lpg=PA164&ots=pv1iGpWJM1&dq=paige%20gls%20algorithm&pg=PA164#v=onepage&q&f=true
  #
  #   The notation in this function follows Bjoerk and ignores the usual
  #   conventions in the tempdisagg

  if (dim(y)[1] <= dim(X)[2]) stop("not enough degrees of freedom")

  # using Bjoerck's notation
  b <- y
  A <- X
  W <- vcov

  # dimensions as in Bjoerck (different from tempdisagg convention)
  m <- dim(A)[1]
  n <- dim(A)[2]

  # Cholesky decomposition of vcov
  B <- t(chol(W))

  # QR decomposition of X, Eq. 4.3.19
  qr.X <- qr(X)
  Q <- qr.Q(qr.X, complete = TRUE)
  R <- qr.R(qr.X)

  # Application to b and B
  .c <- t(Q) %*% b
  c1 <- .c[1:n, ]
  c2 <- .c[(n + 1):m, ]

  .C <- t(Q) %*% B
  C1 <- .C[1:n, ]
  C2 <- .C[(n + 1):m, ]

  # Eq. 4.3.21:
  # transpose C2, flip vertically and horizontally
  tC2 <- t(C2)
  ftC2 <- tC2[dim(tC2)[1]:1, dim(tC2)[2]:1]

  rq.ftC2 <- qr(ftC2)
  PP <- qr.Q(rq.ftC2, complete = TRUE)
  SS <- qr.R(rq.ftC2)

  # flip PP and SS vertically and horizontally, transpose S
  P <- PP[dim(PP)[1]:1, dim(PP)[2]:1]
  S <- t(SS[dim(SS)[1]:1, dim(SS)[2]:1])

  P1 <- P[, 1:n]
  P2 <- P[, (n + 1):m]

  u2 <- matrix(backsolve(S, c2))
  v <- P2 %*% u2

  # coefficients
  x <- backsolve(R, c1 - C1 %*% v)

  # output and stats
  z <- list()
  z$coefficients <- as.numeric(x)

  # generalized RSS
  z$rss <- as.numeric(t(u2) %*% u2)

  if (logl) {
    # standard error of the regression
    z$s_2 <- z$rss / m
    u_l <- y - X %*% z$coefficients
    z$logl <- as.numeric(-m / 2 - m * log(2 * pi) / 2 - m * log(z$s_2) /
      2 - determinant(vcov, logarithm = TRUE)$modulus / 2)
  }

  if (stats) {
    z$s_2_gls <- z$rss / (m - n)

    # vcov: Bjoerck, Eq. 4.3.23
    Lt <- C1 %*% P1
    R_inv <- backsolve(R, diag(n))
    C <- R_inv %*% Lt %*% t(Lt) %*% t(R_inv)

    # standard errors of the coefficients
    z$se <- sqrt(diag(z$s_2_gls * C))

    # total sum of squares                                 TODO: Avoid inverse
    vcov_inv <- solve(vcov)
    e <- matrix(rep(1, m))
    y_bar <- as.numeric(t(e) %*% vcov_inv %*% y / t(e) %*% vcov_inv %*% e)
    z$tss <- as.numeric(t(y - y_bar) %*% vcov_inv %*% (y - y_bar))

    # other stats
    z$rank <- n
    z$df <- m - n
    z$r.squared <- 1 - z$rss / z$tss
    z$adj.r.squared <- 1 - (z$rss * (m - 1)) / (z$tss * (m - n))
    z$aic <- log(z$rss / m) + 2 * (n / m)
    z$bic <- log(z$rss / m) + log(m) * (n / m)
    z$vcov_inv <- vcov_inv
  }
  z
}


CalcDynAdj <- function(X, rho) {
  # Adjust data matrix for dynamic Chow-Lin procedure (Santos-Silva-Cardoso)
  #
  # Args:
  #   X:            a matrix containing the rhs data
  #   rho:          autoregressive parameter
  #
  # Returns:
  #   An updated data matrix, which an additional column
  #
  n <- dim(X)[1]
  diag_rho <- diag(n)
  diag_rho[-1, ] <- diag_rho[-1, ] + (diag(n) * -rho)[-n, ]

  # adjusting data
  solve(diag_rho) %*% cbind(X, c(rho, rep(0, n - 1)))
}
