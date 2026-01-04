# Model Terms of an 'lm_from_lavaan'-Class Object

It extracts the terms object from an `lm_from_lavaan`-class object.

## Usage

``` r
# S3 method for class 'lm_from_lavaan'
terms(x, ...)
```

## Arguments

- x:

  An 'lm_from_lavaan'-class object.

- ...:

  Additional arguments. Ignored.

## Value

A `terms`-class object. See
[terms.object](https://rdrr.io/r/stats/terms.object.html) for details.

## Details

A method for `lm_from_lavaan`-class that converts a regression model for
a variable in a `lavaan` model to a `formula` object. This function
simply calls [`stats::terms()`](https://rdrr.io/r/stats/terms.html) on
the `formula` object to extract the predictors of a variable.

## See also

[terms.object](https://rdrr.io/r/stats/terms.object.html),
[`lm_from_lavaan_list()`](https://sfcheung.github.io/manymome/reference/lm_from_lavaan_list.md)

## Examples

``` r
library(lavaan)
data(data_med)
mod <-
"
m ~ a * x + c1 + c2
y ~ b * m + x + c1 + c2
"
fit <- sem(mod, data_med, fixed.x = FALSE)
fit_list <- lm_from_lavaan_list(fit)
terms(fit_list$m)
#> m ~ x + c1 + c2
#> attr(,"variables")
#> list(m, x, c1, c2)
#> attr(,"factors")
#>    x c1 c2
#> m  0  0  0
#> x  1  0  0
#> c1 0  1  0
#> c2 0  0  1
#> attr(,"term.labels")
#> [1] "x"  "c1" "c2"
#> attr(,"order")
#> [1] 1 1 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: 0x560c278891e0>
terms(fit_list$y)
#> y ~ m + x + c1 + c2
#> attr(,"variables")
#> list(y, m, x, c1, c2)
#> attr(,"factors")
#>    m x c1 c2
#> y  0 0  0  0
#> m  1 0  0  0
#> x  0 1  0  0
#> c1 0 0  1  0
#> c2 0 0  0  1
#> attr(,"term.labels")
#> [1] "m"  "x"  "c1" "c2"
#> attr(,"order")
#> [1] 1 1 1 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: 0x560c27887330>
```
