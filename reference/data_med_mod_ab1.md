# Sample Dataset: Simple Mediation with Both Paths Moderated By a Moderator

A simple mediation model with a-path and b-path moderated by one
moderator.

## Usage

``` r
data_med_mod_ab1
```

## Format

A data frame with 100 rows and 6 variables:

- x:

  Predictor. Numeric.

- w:

  Moderator. Numeric.

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
data(data_med_mod_ab1)
data_med_mod_ab1$xw <-
 data_med_mod_ab1$x *
 data_med_mod_ab1$w
data_med_mod_ab1$mw <-
 data_med_mod_ab1$m *
 data_med_mod_ab1$w
mod <-
"
m ~ a * x + w + da * xw + c1 + c2
y ~ b * m + x + w + db * mw + c1 + c2
w ~~ v_w * w
w ~ m_w * 1
ab := a * b
ab_lo := (a + da * (m_w - sqrt(v_w))) * (b + db * (m_w - sqrt(v_w)))
ab_hi := (a + da * (m_w + sqrt(v_w))) * (b + db * (m_w + sqrt(v_w)))
"
fit <- sem(mod, data_med_mod_ab1,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 9, 38:40), ]
#>      lhs op                                           rhs label    est    se
#> 1      m  ~                                             x     a -1.255 0.164
#> 3      m  ~                                            xw    da  0.264 0.011
#> 6      y  ~                                             m     b -1.276 0.036
#> 9      y  ~                                            mw    db  0.331 0.014
#> 38    ab :=                                           a*b    ab  1.602 0.214
#> 39 ab_lo := (a+da*(m_w-sqrt(v_w)))*(b+db*(m_w-sqrt(v_w))) ab_lo -0.006 0.014
#> 40 ab_hi := (a+da*(m_w+sqrt(v_w)))*(b+db*(m_w+sqrt(v_w))) ab_hi  0.246 0.096
#>          z pvalue ci.lower ci.upper
#> 1   -7.659  0.000   -1.576   -0.934
#> 3   23.531  0.000    0.242    0.286
#> 6  -35.731  0.000   -1.346   -1.206
#> 9   23.119  0.000    0.303    0.359
#> 38   7.489  0.000    1.182    2.021
#> 39  -0.437  0.662   -0.033    0.021
#> 40   2.555  0.011    0.057    0.435
```
