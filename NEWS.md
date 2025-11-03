# tempdisagg 1.2.0

## minor changes

- Improve test coverage and reorganize tests
- Modernize GitHub Actions workflow
- Use `determinant(vcov, logarithm = TRUE)$modulus` for numerical stability
- Suggest xts package for enhanced time series support


# tempdisagg 1.1.1

## minor changes

- Fix tests for R devel


# tempdisagg 1.1 (2023-03-08)

## minor changes

- Use GHA instead of Travis

## bug fixes

- Adjustment to work with latest version of tsbox

## documentation

- ?td: uniform uses criterion = "additive"


# tempdisagg 1.0 (2020-02-07)

## major changes

- works now with most time series classes, as supported by the tsbox package.
- disagregation is possible to all frequencies (e.g., monthly to daily).
  Disaggregation takes into account the calendar, e.g., the fact that February
  is shorter than other months. (#30)
- new method: "fast", a shortcut for chow-lin-fixed with fixed.rho = 0.99999.
  The method returns approximately the same results as "denton-cholette", but is
  much faster. (#14)
- new vignettes: intro to tempdisagg, disaggregation to high frequency

## under the hood

- supports three modes: tsbox, ts, numeric
- markdown in roxygen, NEWS.md
- testthat infrastructure


# tempdisagg 0.25 (2016-07-10)

## changes visible to the user

- new methods: "dynamic-maxlog", "dynamic-minrss", "dynamic-fixed", as described in
  Santos Silva and Cardoso, 2001. Many thanks to Tommaso Di Fonzo for providing
  a blueprint written in GAUSS.
- updated documentation to include new methods.

## minor changes

- better checks for non-time-series inputs.
  (https://github.com/christophsax/tempdisagg/issues/20)
- added extensive numerical testing on travis.

## bug fixes

- ta() returns correct results if conversion is "last" or "first", and the first
  or the last period is incomplete.
  (https://github.com/christophsax/tempdisagg/issues/22)


# tempdisagg 0.24 (2014-12-07)

## changes visible to the user:
- retropolation: 'td' will performs both extra- and retropolation if the high
  frequency series covers a larger time span than the low frequency series.
- low frequency values are ignored if series is longer than high frequency
  series (with a warning).
- suggestion to use 'denton-cholette' when the original 'denton' method is
  chosen.


# tempdisagg 0.23 (2014-01-11)

## changes visible to the user

- Our R-Journal article on temporal disaggregation explains tempdisagg in more
  detail. Links are included in the package description, the help files and
  the README file.

## minor changes

- warning in ta() if a time series contains internal NAs.
- formating tweaks in the help files.


# tempdisagg 0.22 (2013-08-07)

## changes visible to the user

- predict method for 'td' is now different from fitted:
  - $fitted.values of a 'td' object now containts the low-frequency fitted
    values of a  regression or the low-frequency indicator in case of the
    Denton methods. The values are be accessed by fitted().
  - The final high frequency series is now stored in $values. As before, these
    values are accessed by predict().
- Package overview (?tempdisagg)
- Demo (demo(tempdisagg))
- argument 'truncated.rho = 0' instead of 'no.neg = TRUE'. This allows for
  truncation values different from 0. Default behavior is the same as in 0.21.

## bug fixes

- in 0.21, ta() produced an error if less than a low-frequency unit was
  covered by high frequency data. Now it produces series containing only NA.
- If a singular data matrix is entered, there is a new warning.


# tempdisagg 0.21 (2013-01-21)

## changes visible to the user

- new methods available: "chow-lin-fixed" and "litterman-fixed".
  Using the "fixed.rho" argument, an autoregressive parameter may be specified
  by the user.
- interface changes: "chow-lin-maxlog-ecotrim" and "chow-lin-maxlog-quilis"
  are defined as new methods. No need for the old 'vcov' argument anymore.
- new defaults: method = "chow-lin-maxlog", neg.rho = FALSE
  with positive values for rho only, the chow-lin-maxlog method generally
  outperforms the other methods.
- all relevant arguments are directly entered to td()
- summary output: If neg.rho = FALSE and a negative rho is truncated to 0, and
  indicator is shown in the summary output.
- non time-series mode: optionally, standard vectors can be used instead of
  time series. In this case, the frequency of low frequency variable is 1,
  while the fraction of the high frequency variable is specified by the 'to'
  argument
- updated help files

## under the hood

- td() is rewritten and has a clear structure now.
- GLS Regressions are performed by the new CalcGLS() function, which uses
  QR-decomposition instead of matrix-inversion. This is faster and
  numerically stable. It resolves an issue wher large (or small) numbers have
  led to a 'system is computationally singular' error.
