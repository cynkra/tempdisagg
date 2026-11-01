# Residual Plot for Temporal Disaggregation

`plot` method for class `"td"`. Plot the fitted and actual low frequency
series, and residuals.

## Usage

``` r
# S3 method for class 'td'
plot(x, ...)
```

## Arguments

- x:

  an object of class `"td"`, usually, a result of a call to
  [`td()`](http://cynkra.github.io/tempdisagg/reference/td.md).

- ...:

  further arguments passed to or from other methods.

## Value

returns a a two panel plot as its side effect, showing the fitted and
actual low frequency series, and the residuals.

## See also

[`td()`](http://cynkra.github.io/tempdisagg/reference/td.md) for the
main function for temporal disaggregation.

## Examples

``` r
data(swisspharma)

mod2 <- td(sales.a ~ imports.q + exports.q)
plot(mod2)
```
