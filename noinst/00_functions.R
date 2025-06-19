########################################################
# Test, generieren der Zeitreihen mit R (tempdisagg)
########################################################

# function for all variants to be compared
# -----------------------------------------------------------------------------
estimAll <- function(formula1=formula1, formula2=formula2, formula3=formula3, freq=freq, pre=FALSE){
  R <- list()
  if(pre) {# pre=TRUE for tempdisagg versions < 0.22
    R$cl_rss_R <- td(formula1, method="chow-lin-minrss-ecotrim", neg.rho=TRUE)
    R$cl_rss_Q <- td(formula1, method="chow-lin-minrss-quilis", neg.rho=TRUE)
    R$cl_log <- td(formula1, method="chow-lin-maxlog", neg.rho=TRUE)
    R$fer <- td(formula1, method="fernandez", neg.rho=TRUE)
    R$lit_rss <- td(formula1, method="litterman-minrss", neg.rho=TRUE)
    R$lit_log <- td(formula1, method="litterman-maxlog", neg.rho=TRUE)
  } else {# for tempdisagg versions 0.22 or higher
    R$cl_rss_R <- td(formula1, method="chow-lin-minrss-ecotrim", truncated.rho=-1)
    R$cl_rss_Q <- td(formula1, method="chow-lin-minrss-quilis", truncated.rho=-1)
    R$cl_log <- td(formula1, method="chow-lin-maxlog", truncated.rho=-1)
    R$fer <- td(formula1, method="fernandez", truncated.rho=-1)
    R$lit_rss <- td(formula1, method="litterman-minrss", truncated.rho=-1)
    R$lit_log <- td(formula1, method="litterman-maxlog", truncated.rho=-1)
  }
  R$dencho_p_0 <- td(formula2, method="denton-cholette", h=0, to=freq)
  R$dencho_p_1 <- td(formula2, method="denton-cholette", h=1, to=freq)
  R$dencho_p_2 <- td(formula2, method="denton-cholette", h=2, to=freq)
  R$dencho_a_0 <- td(formula2, method="denton-cholette", h=0, criterion="additive", to=freq)
  R$dencho_a_1 <- td(formula2, method="denton-cholette", h=1, criterion="additive", to=freq)
  R$dencho_a_2 <- td(formula2, method="denton-cholette", h=2, criterion="additive", to=freq)
  R$den_p_0 <- td(formula2, method="denton", h=0, to=freq)
  R$den_p_1 <- td(formula2, method="denton", h=1, to=freq)
  R$den_p_2 <- td(formula2, method="denton", h=2, to=freq)
  R$den_a_0 <- td(formula2, method="denton", h=0, criterion="additive", to=freq)
  R$den_a_1 <- td(formula2, method="denton", h=1, criterion="additive", to=freq)
  R$den_a_2 <- td(formula2, method="denton", h=2, criterion="additive", to=freq)
  R$dencho_ind_p_0 <- td(formula3, method="denton-cholette", h=0)
  R$dencho_ind_p_1 <- td(formula3, method="denton-cholette", h=1)
  R$dencho_ind_p_2 <- td(formula3, method="denton-cholette", h=2)
  R$dencho_ind_a_0 <- td(formula3, method="denton-cholette", h=0, criterion="additive")
  R$dencho_ind_a_1 <- td(formula3, method="denton-cholette", h=1, criterion="additive")
  R$dencho_ind_a_2 <- td(formula3, method="denton-cholette", h=2, criterion="additive")
  R$den_ind_p_0 <- td(formula3, method="denton", h=0)
  R$den_ind_p_1 <- td(formula3, method="denton", h=1)
  R$den_ind_p_2 <- td(formula3, method="denton", h=2)
  R$den_ind_a_0 <- td(formula3, method="denton", h=0, criterion="additive")
  R$den_ind_a_1 <- td(formula3, method="denton", h=1, criterion="additive")
  R$den_ind_a_2 <- td(formula3, method="denton", h=2, criterion="additive")
  R$ols <- td(formula1, method="ols")
  R$cl_fix <- td(formula1, method="chow-lin-fixed", fixed.rho=0.6)
  R$lit_fix <- td(formula1, method="litterman-fixed", fixed.rho=0.6)
  R$uni <- td(formula2, method="uniform", to=freq)
  R
}

compareNewOld <- function(new, old){
  if(any(colnames(new) != colnames(old))) {new <- new[, colnames(old)]}
  all.equal(new, old)
  identical(new, old)
}

diffsNewOld <- function(new, old){
  if(any(colnames(new) != colnames(old))) {new <- new[, colnames(old)]}
  diffs <- new - old
  colnames(diffs) <- gsub("new\\.", "", colnames(diffs))
  comp.max <- apply(diffs, MARGIN=2, FUN=max, na.rm=TRUE)
  comp.min <- apply(diffs, MARGIN=2, FUN=min, na.rm=TRUE)
  cbind(comp.max,comp.min)
}

