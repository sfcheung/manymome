# Sample Dataset: Serial Mediation with Two Moderators

A simple mediation model with a-path and b2-path moderated.

## Usage

``` r
data_med_mod_serial
```

## Format

A data frame with 100 rows and 8 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator 1. Numeric.

- w2:

  Moderator 2. Numeric.

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
data(data_med_mod_serial)
data_med_mod_serial$xw1 <-
 data_med_mod_serial$x *
 data_med_mod_serial$w1
data_med_mod_serial$m2w2 <-
 data_med_mod_serial$m2 *
 data_med_mod_serial$w2
mod <-
"
m1 ~ a * x + w1 + da1 * xw1 + c1 + c2
m2 ~ b1 * m1 + x + w1 + c1 + c2
y ~ b2 * m2 + m1 + x + w1 + w2 + db2 * m2w2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
ab1b2 := a * b1 * b2
ab1b2_lolo := (a + da1 * (m_w1 - sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
ab1b2_lohi := (a + da1 * (m_w1 - sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
ab1b2_hilo := (a + da1 * (m_w1 + sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
ab1b2_hihi := (a + da1 * (m_w1 + sqrt(v_w1))) * b1 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
"
fit <- sem(mod, data_med_mod_serial,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 11, 16, 49:53), ]
#>           lhs op                                                     rhs
#> 1          m1  ~                                                       x
#> 3          m1  ~                                                     xw1
#> 6          m2  ~                                                      m1
#> 11          y  ~                                                      m2
#> 16          y  ~                                                    m2w2
#> 49      ab1b2 :=                                                 a*b1*b2
#> 50 ab1b2_lolo := (a+da1*(m_w1-sqrt(v_w1)))*b1*(b2+db2*(m_w2-sqrt(v_w2)))
#> 51 ab1b2_lohi := (a+da1*(m_w1-sqrt(v_w1)))*b1*(b2+db2*(m_w2+sqrt(v_w2)))
#> 52 ab1b2_hilo := (a+da1*(m_w1+sqrt(v_w1)))*b1*(b2+db2*(m_w2-sqrt(v_w2)))
#> 53 ab1b2_hihi := (a+da1*(m_w1+sqrt(v_w1)))*b1*(b2+db2*(m_w2+sqrt(v_w2)))
#>         label    est    se      z pvalue ci.lower ci.upper
#> 1           a -1.328 0.136 -9.741  0.000   -1.595   -1.061
#> 3         da1  0.289 0.010 28.738  0.000    0.270    0.309
#> 6          b1  0.183 0.032  5.712  0.000    0.120    0.246
#> 11         b2 -0.469 0.092 -5.075  0.000   -0.651   -0.288
#> 16        db2  0.333 0.007 45.421  0.000    0.319    0.348
#> 49      ab1b2  0.114 0.032  3.535  0.000    0.051    0.177
#> 50 ab1b2_lolo -0.009 0.006 -1.381  0.167   -0.021    0.004
#> 51 ab1b2_lohi -0.033 0.019 -1.711  0.087   -0.071    0.005
#> 52 ab1b2_hilo  0.018 0.010  1.892  0.058   -0.001    0.037
#> 53 ab1b2_hihi  0.070 0.022  3.219  0.001    0.027    0.112
```
