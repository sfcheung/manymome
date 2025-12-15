# Sample Dataset: Two Moderators

A two-moderator model.

## Usage

``` r
data_mod2
```

## Format

A data frame with 100 rows and 6 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator 1. Numeric.

- w2:

  Moderator 2. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_mod2)
data_mod2$xw1 <- data_mod2$x * data_mod2$w1
data_mod2$xw2 <- data_mod2$x * data_mod2$w2
mod <-
"
y ~ a * x + w1 + w2 + d1 * xw1 + d2 * xw2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
a_lolo := a + d1 * (m_w1 - sqrt(v_w1)) + d2 * (m_w2 - sqrt(v_w2))
a_lohi := a + d1 * (m_w1 - sqrt(v_w1)) + d2 * (m_w2 + sqrt(v_w2))
a_hilo := a + d1 * (m_w1 + sqrt(v_w1)) + d2 * (m_w2 - sqrt(v_w2))
a_hihi := a + d1 * (m_w1 + sqrt(v_w1)) + d2 * (m_w2 + sqrt(v_w2))
"
fit <- sem(mod, data_mod2, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 4, 5, 8:11, 34:37), ]
#>       lhs op                                         rhs  label    est    se
#> 1       y  ~                                           x      a  0.716 0.098
#> 4       y  ~                                         xw1     d1  0.107 0.009
#> 5       y  ~                                         xw2     d2 -0.191 0.009
#> 8      w1 ~~                                          w1   v_w1  0.987 0.140
#> 9      w1 ~1                                               m_w1  4.027 0.099
#> 10     w2 ~~                                          w2   v_w2  1.052 0.149
#> 11     w2 ~1                                               m_w2  2.959 0.103
#> 34 a_lolo := a+d1*(m_w1-sqrt(v_w1))+d2*(m_w2-sqrt(v_w2)) a_lolo  0.671 0.091
#> 35 a_lohi := a+d1*(m_w1-sqrt(v_w1))+d2*(m_w2+sqrt(v_w2)) a_lohi  0.279 0.089
#> 36 a_hilo := a+d1*(m_w1+sqrt(v_w1))+d2*(m_w2-sqrt(v_w2)) a_hilo  0.884 0.091
#> 37 a_hihi := a+d1*(m_w1+sqrt(v_w1))+d2*(m_w2+sqrt(v_w2)) a_hihi  0.492 0.090
#>          z pvalue ci.lower ci.upper
#> 1    7.286  0.000    0.523    0.908
#> 4   12.461  0.000    0.090    0.124
#> 5  -22.359  0.000   -0.208   -0.174
#> 8    7.071  0.000    0.713    1.260
#> 9   40.545  0.000    3.833    4.222
#> 10   7.071  0.000    0.761    1.344
#> 11  28.845  0.000    2.758    3.160
#> 34   7.360  0.000    0.492    0.850
#> 35   3.134  0.002    0.105    0.454
#> 36   9.663  0.000    0.705    1.063
#> 37   5.481  0.000    0.316    0.668
```
