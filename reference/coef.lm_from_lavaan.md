# Coefficients of an 'lm_from_lavaan'-Class Object

Returns the path coefficients of the terms in an `lm_from_lavaan`-class
object.

## Usage

``` r
# S3 method for class 'lm_from_lavaan'
coef(object, ...)
```

## Arguments

- object:

  A 'lm_from_lavaan'-class object.

- ...:

  Additional arguments. Ignored.

## Value

A numeric vector of the path coefficients.

## Details

An `lm_from_lavaan`-class object converts a regression model for a
variable in a `lavaan`-class object to a `formula`-class object. This
function simply extracts the path coefficients estimates. Intercept is
always included, and set to zero if mean structure is not in the source
`lavaan`-class object.

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
coef(fit_list$m)
#> (Intercept)           x          c1          c2 
#>   0.0000000   0.9346863   0.1977842  -0.1684097 
coef(fit_list$y)
#> (Intercept)           m           x          c1          c2 
#>   0.0000000   0.7846871   0.5076728   0.1404921  -0.1543881 
```
