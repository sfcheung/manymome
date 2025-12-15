# Predicted Values of an 'lm_from_lavaan_list'-Class Object

It computes the predicted values based on the models stored in an
'lm_from_lavaan_list\`-class object.

## Usage

``` r
# S3 method for class 'lm_from_lavaan_list'
predict(object, x = NULL, y = NULL, m = NULL, newdata, ...)
```

## Arguments

- object:

  A 'lm_from_lavaan'-class object.

- x:

  The variable name at the start of a path.

- y:

  The variable name at the end of a path.

- m:

  Optional. The mediator(s) from `x` to `y`. A numeric vector of the
  names of the mediators. The path goes from the first element to the
  last element. For example, if `m = c("m1", "m2")`, then the path is
  `x -> m1 -> m2 -> y`.

- newdata:

  Required. A data frame of the new data. It must be a data frame.

- ...:

  Additional arguments. Ignored.

## Value

A numeric vector of the predicted values, with length equal to the
number of rows of user-supplied data.

## Details

An `lm_from_lavaan_list`-class object is a list of
`lm_from_lavaan`-class objects.

This is an advanced helper used by
[`plot.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/plot.cond_indirect_effects.md).
Exported for advanced users and developers.

## See also

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
tmp <- data.frame(x = 1, c1 = 2, c2 = 3, m = 4)
predict(fit_list, x = "x", y = "y", m = "m", newdata = tmp)
#> [1] 0.9728797
```
