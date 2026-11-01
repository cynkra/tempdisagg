# Temporal Aggregation of Time Series

Performs temporal aggregation of high to low frequency time series.
Currently, `ta` only works with `ts` or `mts` time series objects.

## Usage

``` r
ta(x, ...)

# S3 method for class 'ts'
ta(x, conversion = "sum", to = "annual", ...)
```

## Arguments

- x:

  a time series object of class `"ts"` or `"mts"`.

- ...:

  additional arguments, passed to the methods.

- conversion:

  type of conversion: `"sum"`, `"average"`, `"first"` or `"last"`.

- to:

  (low-frequency) destination frequency as a character string
  (`"annual"` or `"quarterly"`) or as a scalar (e.g. `1`, `2`, `4`).

## Value

`ta` returns an object of class `"ts"` or `"mts"`, depending on the
class of the input series.

## Details

`ta` is used to aggregate a high frequency time series into a low
frequency series, while the latter is either the sum, the average, the
first or the last value of the high-frequency series. `ta` is the
inverse function of
[`td()`](http://cynkra.github.io/tempdisagg/reference/td.md). If applied
to an output series of `td`, `ta` yields the original series.

## See also

[`td()`](http://cynkra.github.io/tempdisagg/reference/td.md) for the
main function for temporal disaggregation.

## Examples

``` r
data(swisspharma)

sales.q.a <- ta(sales.q, conversion = "sum", to = "annual")
all.equal(sales.a, sales.q.a)
#> [1] TRUE
```
