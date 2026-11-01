# Summary of a Temporal Disaggregation

`summary` method for class "td".

## Usage

``` r
# S3 method for class 'td'
summary(object, ...)

# S3 method for class 'summary.td'
print(
  x,
  digits = max(3, getOption("digits") - 3),
  signif.stars = getOption("show.signif.stars"),
  ...
)
```

## Arguments

- object:

  an object of class `"td"`, usually, a result of a call to
  [`td()`](http://cynkra.github.io/tempdisagg/reference/td.md).

- ...:

  further arguments passed to or from other methods.

- x:

  an object of class `"summary.td"`, usually, a result of a call to
  `summary.td`.

- digits:

  the number of significant digits to use when printing.

- signif.stars:

  logical. If `TRUE`, 'significance stars' are printed for each
  coefficient.

## Value

`summary.td` returns a list containing the summary statistics included
in `object`, and computes the following additional statistics:

- n_l:

  number of low frequency observations

- n:

  number of high frequency observations

- ar_l:

  empirical auto-correlation of the low frequency series

- coefficients:

  a named matrix containing coefficients, standard deviations, t-values
  and p-values

The `print` method prints the summary output in a similar way as the
method for `"lm"`.

## See also

[`td()`](http://cynkra.github.io/tempdisagg/reference/td.md) for the
main function for temporal disaggregation.

## Examples

``` r
data(swisspharma)

mod1 <- td(sales.a ~ imports.q + exports.q)
summary(mod1)
#> 
#> Call:
#> td(formula = sales.a ~ imports.q + exports.q)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -61.648  -7.139  -2.825   5.508  53.373 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 11.685855   1.493077   7.827 5.08e-09 ***
#> imports.q    0.003934   0.002113   1.862   0.0716 .  
#> exports.q    0.011258   0.001158   9.723 3.26e-11 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> 'chow-lin-maxlog' disaggregation with 'sum' conversion
#> 36 low-freq. obs. converted to 158 high-freq. obs.
#> Adjusted R-squared: 0.9949   AR1-Parameter:     0 (truncated)

mod2 <- td(sales.a ~ 0, to = "quarterly", method = "uniform")
summary(mod2)
#> 
#> Call:
#> td(formula = sales.a ~ 0, to = "quarterly", method = "uniform")
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#>  132.7  205.6  330.9  625.9 1041.6 
#> 
#> No Coefficients
#> 
#> 'uniform' disaggregation with 'sum' conversion
#> 36 low-freq. obs. converted to 144 high-freq. obs.
#> criterion: additive  order of differencing 'h': 0
```
