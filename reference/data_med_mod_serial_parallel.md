# Sample Dataset: Serial-Parallel Mediation with Two Moderators

A serial-parallel mediation model with some paths moderated.

## Usage

``` r
data_med_mod_serial_parallel
```

## Format

A data frame with 100 rows and 9 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator 1. Numeric.

- w2:

  Moderator 2. Numeric.

- m11:

  Mediator 1 in Path 1. Numeric.

- m12:

  Mediator 2 in Path 2. Numeric.

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
data(data_med_mod_serial_parallel)
data_med_mod_serial_parallel$xw1 <-
 data_med_mod_serial_parallel$x *
 data_med_mod_serial_parallel$w1
data_med_mod_serial_parallel$m2w2 <-
 data_med_mod_serial_parallel$m2 *
 data_med_mod_serial_parallel$w2
mod <-
"
m11 ~ a1 * x + w1 + da11 * xw1 + c1 + c2
m12 ~ b11 * m11 + x + w1 + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b12 * m12 + b2 * m2 + m11 + x + w1 + w2 + db2 * m2w2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
a1b11b22 := a1 * b11 * b12
a2b2 := a2 * b2
ab := a1b11b22 + a2b2
a1b11b12_w1lo := (a1 + da11 * (m_w1 - sqrt(v_w1))) * b11 * b12
a1b11b12_w1hi := (a1 + da11 * (m_w1 + sqrt(v_w1))) * b11 * b12
a2b2_w2lo := a2 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
a2b2_w2hi := a2 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
"
fit <- sem(mod, data_med_mod_serial_parallel,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[parameterEstimates(fit)$label != "", ]
#>              lhs op                                 rhs         label    est
#> 1            m11  ~                                   x            a1 -1.328
#> 3            m11  ~                                 xw1          da11  2.894
#> 6            m12  ~                                 m11           b11  0.248
#> 11            m2  ~                                   x            a2  0.349
#> 14             y  ~                                 m12           b12  0.320
#> 15             y  ~                                  m2            b2 -0.711
#> 20             y  ~                                m2w2           db2  4.057
#> 23            w1 ~~                                  w1          v_w1  0.012
#> 24            w1 ~1                                              m_w1  0.497
#> 25            w2 ~~                                  w2          v_w2  0.010
#> 26            w2 ~1                                              m_w2  0.304
#> 55      a1b11b22 :=                          a1*b11*b12      a1b11b22 -0.105
#> 56          a2b2 :=                               a2*b2          a2b2 -0.249
#> 57            ab :=                       a1b11b22+a2b2            ab -0.354
#> 58 a1b11b12_w1lo := (a1+da11*(m_w1-sqrt(v_w1)))*b11*b12 a1b11b12_w1lo -0.016
#> 59 a1b11b12_w1hi := (a1+da11*(m_w1+sqrt(v_w1)))*b11*b12 a1b11b12_w1hi  0.034
#> 60     a2b2_w2lo :=       a2*(b2+db2*(m_w2-sqrt(v_w2)))     a2b2_w2lo  0.044
#> 61     a2b2_w2hi :=       a2*(b2+db2*(m_w2+sqrt(v_w2)))     a2b2_w2hi  0.322
#>       se      z pvalue ci.lower ci.upper
#> 1  0.136 -9.741  0.000   -1.595   -1.061
#> 3  0.101 28.738  0.000    2.697    3.091
#> 6  0.014 17.421  0.000    0.220    0.276
#> 11 0.034 10.154  0.000    0.282    0.417
#> 14 0.216  1.478  0.139   -0.104    0.744
#> 15 0.189 -3.768  0.000   -1.081   -0.341
#> 20 0.156 25.993  0.000    3.751    4.363
#> 23 0.002  7.071  0.000    0.009    0.015
#> 24 0.011 45.550  0.000    0.476    0.519
#> 25 0.001  7.071  0.000    0.007    0.012
#> 26 0.010 31.028  0.000    0.285    0.323
#> 55 0.072 -1.456  0.145   -0.247    0.036
#> 56 0.070 -3.533  0.000   -0.386   -0.111
#> 57 0.101 -3.507  0.000   -0.552   -0.156
#> 58 0.014 -1.149  0.251   -0.044    0.011
#> 59 0.024  1.396  0.163   -0.014    0.081
#> 60 0.069  0.633  0.527   -0.092    0.179
#> 61 0.078  4.111  0.000    0.168    0.475
```
