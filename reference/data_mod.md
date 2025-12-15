# Sample Dataset: One Moderator

A one-moderator model.

## Usage

``` r
data_mod
```

## Format

A data frame with 100 rows and 5 variables:

- x:

  Predictor. Numeric.

- w:

  Moderator. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_mod)
data_mod$xw <- data_mod$x * data_mod$w
mod <-
"
y ~ a * x + w + d * xw + c1 + c2
w ~~ v_w * w
w ~ m_w * 1
a_lo := a + d * (m_w - sqrt(v_w))
a_hi := a + d * (m_w + sqrt(v_w))
"
fit <- sem(mod, data_mod, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 7, 24, 25), ]
#>     lhs op                 rhs label   est    se      z pvalue ci.lower
#> 1     y  ~                   x     a 0.453 0.235  1.924  0.054   -0.008
#> 3     y  ~                  xw     d 0.596 0.020 29.951  0.000    0.557
#> 6     w ~~                   w   v_w 1.137 0.161  7.071  0.000    0.822
#> 7     w ~1                       m_w 3.213 0.107 30.142  0.000    3.005
#> 24 a_lo := a+d*(m_w-sqrt(v_w))  a_lo 1.733 0.244  7.113  0.000    1.256
#> 25 a_hi := a+d*(m_w+sqrt(v_w))  a_hi 3.004 0.247 12.178  0.000    2.521
#>    ci.upper
#> 1     0.915
#> 3     0.635
#> 6     1.452
#> 7     3.422
#> 24    2.211
#> 25    3.487
```
