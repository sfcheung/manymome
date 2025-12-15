# Sample Dataset: Simple Mediation with Both Paths Moderated (Two Moderators)

A simple mediation model with a-path and b-path each moderated by a
moderator.

## Usage

``` r
data_med_mod_ab
```

## Format

A data frame with 100 rows and 7 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator 1. Numeric.

- w2:

  Moderator 2. Numeric.

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
data(data_med_mod_ab)
data_med_mod_ab$xw1 <-
 data_med_mod_ab$x *
 data_med_mod_ab$w1
data_med_mod_ab$mw2 <-
 data_med_mod_ab$m *
 data_med_mod_ab$w2
mod <-
"
m ~ a * x + w1 + d1 * xw1 + c1 + c2
y ~ b * m + x + w1 + w2 + d2 * mw2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
ab := a * b
ab_lolo := (a + d1 * (m_w1 - sqrt(v_w1))) * (b + d2 * (m_w2 - sqrt(v_w2)))
ab_lohi := (a + d1 * (m_w1 - sqrt(v_w1))) * (b + d2 * (m_w2 + sqrt(v_w2)))
ab_hilo := (a + d1 * (m_w1 + sqrt(v_w1))) * (b + d2 * (m_w2 - sqrt(v_w2)))
ab_hihi := (a + d1 * (m_w1 + sqrt(v_w1))) * (b + d2 * (m_w2 + sqrt(v_w2)))
"
fit <- sem(mod, data_med_mod_ab,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 10, 41:45), ]
#>        lhs op                                               rhs   label    est
#> 1        m  ~                                                 x       a -0.663
#> 3        m  ~                                               xw1      d1  0.204
#> 6        y  ~                                                 m       b -0.150
#> 10       y  ~                                               mw2      d2  0.203
#> 41      ab :=                                               a*b      ab  0.099
#> 42 ab_lolo := (a+d1*(m_w1-sqrt(v_w1)))*(b+d2*(m_w2-sqrt(v_w2))) ab_lolo  0.043
#> 43 ab_lohi := (a+d1*(m_w1-sqrt(v_w1)))*(b+d2*(m_w2+sqrt(v_w2))) ab_lohi  0.108
#> 44 ab_hilo := (a+d1*(m_w1+sqrt(v_w1)))*(b+d2*(m_w2-sqrt(v_w2))) ab_hilo  0.159
#> 45 ab_hihi := (a+d1*(m_w1+sqrt(v_w1)))*(b+d2*(m_w2+sqrt(v_w2))) ab_hihi  0.397
#>       se      z pvalue ci.lower ci.upper
#> 1  0.179 -3.700  0.000   -1.014   -0.312
#> 3  0.012 16.774  0.000    0.180    0.228
#> 6  0.048 -3.115  0.002   -0.244   -0.056
#> 10 0.012 16.258  0.000    0.178    0.227
#> 41 0.042  2.383  0.017    0.018    0.181
#> 42 0.039  1.100  0.271   -0.034    0.120
#> 43 0.095  1.129  0.259   -0.079    0.295
#> 44 0.050  3.173  0.002    0.061    0.257
#> 45 0.095  4.175  0.000    0.211    0.584
```
