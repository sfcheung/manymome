# Sample Dataset: Serial Mediation

A serial mediation model.

## Usage

``` r
data_serial
```

## Format

A data frame with 100 rows and 6 variables:

- x:

  Predictor. Numeric.

- m1:

  Mediator 1. Numeric.

- m2:

  Mediator 2. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_serial)
mod <-
"
m1 ~ a * x + c1 + c2
m2 ~ b1 * m1 + x + c1 + c2
y ~ b2 * m2 + m1 + x + c1 + c2
indirect := a * b1 * b2
"
fit <- sem(mod, data_serial,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 4, 8, 28), ]
#>         lhs op     rhs    label   est    se     z pvalue ci.lower ci.upper
#> 1        m1  ~       x        a 0.822 0.092 8.907  0.000    0.641    1.003
#> 4        m2  ~      m1       b1 0.421 0.099 4.237  0.000    0.226    0.615
#> 8         y  ~      m2       b2 0.521 0.221 2.361  0.018    0.088    0.953
#> 28 indirect := a*b1*b2 indirect 0.180 0.090 2.009  0.045    0.004    0.356
```
