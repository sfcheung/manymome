# Check a Path Exists in a Model

It checks whether a path, usually an indirect path, exists in a model.

## Usage

``` r
check_path(x, y, m = NULL, fit = NULL, est = NULL)
```

## Arguments

- x:

  Character. The name of predictor at the start of the path.

- y:

  Character. The name of the outcome variable at the end of the path.

- m:

  A vector of the variable names of the mediators. The path goes from
  the first mediator successively to the last mediator. If `NULL`, the
  default, the path goes from `x` to `y`.

- fit:

  The fit object. Currently only supports a `lavaan::lavaan-class`
  object or a list of outputs of
  [`lm()`](https://rdrr.io/r/stats/lm.html). It can also be a
  `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).
  If it is a single model fitted by
  [`lm()`](https://rdrr.io/r/stats/lm.html), it will be automatically
  converted to a list by
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

- est:

  The output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  If `NULL`, the default, it will be generated from `fit`. If supplied,
  `fit` will ge ignored.

## Value

A logical vector of length one. `TRUE` if the path is valid, `FALSE` if
the path is invalid.

## Details

It checks whether the path defined by a predictor (`x`), an outcome
(`y`), and optionally a sequence of mediators (`m`), exists in a model.
It can check models in a `lavaan::lavaan-class` object or a list of
outputs of [`lm()`](https://rdrr.io/r/stats/lm.html). It also support
`lavaan.mi` objects returned by
[`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
or its wrapper, such as
[`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

For example, in the following model in `lavaan` syntax

    m1 ~ x
    m2 ~ m1
    m3 ~ x
    y ~ m2 + m3

This path is valid: `x = "x", y = "y", m = c("m1", "m2")`

This path is invalid: `x = "x", y = "y", m = c("m2")`

This path is also invalid: `x = "x", y = "y", m = c("m1", "m2")`

## Examples

``` r
library(lavaan)
data(data_serial_parallel)
dat <- data_serial_parallel
mod <-
"
m11 ~ x + c1 + c2
m12 ~ m11 + x + c1 + c2
m2 ~ x + c1 + c2
y ~ m12 + m2 + m11 + x + c1 + c2
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE)

# The following paths are valid
check_path(x = "x", y = "y", m = c("m11", "m12"), fit = fit)
#> [1] TRUE
check_path(x = "x", y = "y", m = "m2", fit = fit)
#> [1] TRUE
# The following paths are invalid
check_path(x = "x", y = "y", m = c("m11", "m2"), fit = fit)
#> [1] FALSE
check_path(x = "x", y = "y", m = c("m12", "m11"), fit = fit)
#> [1] FALSE
```
