# 'lavaan'-class to 'lm_from_lavaan_list'-Class

Converts the regression models in a `lavaan`-class model to an
`lm_from_lavaan_list`-class object.

## Usage

``` r
lm_from_lavaan_list(fit)
```

## Arguments

- fit:

  A `lavaan`-class object, usually the output of
  [`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html) or
  its wrappers.

## Value

An `lm_from_lavaan_list`-class object, which is a list of
`lm_from_lavaan` objects. It has a `predict`-method
([`predict.lm_from_lavaan_list()`](https://sfcheung.github.io/manymome/reference/predict.lm_from_lavaan_list.md))
for computing the predicted values from one variable to another.

## Details

It identifies all dependent variables in a `lavaan` model and creates an
`lm_from_lavaan`-class object for each of them.

This is an advanced helper used by
[`plot.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/plot.cond_indirect_effects.md).
Exported for advanced users and developers.

## See also

[predict.lm_from_lavaan_list](https://sfcheung.github.io/manymome/reference/predict.lm_from_lavaan_list.md)

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
