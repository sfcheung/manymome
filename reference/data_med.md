# Sample Dataset: Simple Mediation

A simple mediation model.

## Usage

``` r
data_med
```

## Format

A data frame with 100 rows and 5 variables:

- x:

  Predictor. Numeric.

- m:

  Mediator. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_med)
mod <-
"
m ~ a * x + c1 + c2
y ~ b * m + x + c1 + c2
ab := a * b
"
fit <- sem(mod, data_med, fixed.x = FALSE)
parameterEstimates(fit)
#>    lhs op rhs label    est    se      z pvalue ci.lower ci.upper
#> 1    m  ~   x     a  0.935 0.079 11.802  0.000    0.779    1.090
#> 2    m  ~  c1        0.198 0.075  2.629  0.009    0.050    0.345
#> 3    m  ~  c2       -0.168 0.101 -1.668  0.095   -0.366    0.029
#> 4    y  ~   m     b  0.785 0.243  3.227  0.001    0.308    1.261
#> 5    y  ~   x        0.508 0.298  1.704  0.088   -0.076    1.092
#> 6    y  ~  c1        0.140 0.189  0.743  0.458   -0.230    0.511
#> 7    y  ~  c2       -0.154 0.249 -0.620  0.535   -0.642    0.334
#> 8    m ~~   m        0.681 0.096  7.071  0.000    0.493    0.870
#> 9    y ~~   y        4.030 0.570  7.071  0.000    2.913    5.147
#> 10   x ~~   x        1.102 0.156  7.071  0.000    0.797    1.408
#> 11   x ~~  c1        0.026 0.116  0.221  0.825   -0.202    0.253
#> 12   x ~~  c2        0.100 0.087  1.143  0.253   -0.071    0.271
#> 13  c1 ~~  c1        1.218 0.172  7.071  0.000    0.880    1.555
#> 14  c1 ~~  c2       -0.092 0.092 -1.002  0.316   -0.272    0.088
#> 15  c2 ~~  c2        0.685 0.097  7.071  0.000    0.495    0.875
#> 16  ab := a*b    ab  0.733 0.236  3.112  0.002    0.272    1.195
```
