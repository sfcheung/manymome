# Predicted Values of a 'lm_from_lavaan'-Class Object

Compute the predicted values based on the model stored in a
'lm_from_lavaan\`-class object.

## Usage

``` r
# S3 method for class 'lm_from_lavaan'
predict(object, newdata, ...)
```

## Arguments

- object:

  A 'lm_from_lavaan'-class object.

- newdata:

  Required. A data frame of the new data. It must be a data frame.

- ...:

  Additional arguments. Ignored.

## Value

A numeric vector of the predicted values, with length equal to the
number of rows of user-supplied data.

## Details

An `lm_from_lavaan`-class method that converts a regression model for a
variable in a `lavaan` model to a `formula` object. This function uses
the stored model to compute predicted values using user-supplied data.

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
predict(fit_list$m, newdata = tmp)
#> [1] 0.8250255
predict(fit_list$y, newdata = tmp)
#> [1] 3.464241
```
