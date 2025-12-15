# Sample Dataset: Simple Mediation with b-Path Moderated

A simple mediation model with b-path moderated.

## Usage

``` r
data_med_mod_b
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
data(data_med_mod_b)
data_med_mod_b$mw <-
 data_med_mod_b$m *
 data_med_mod_b$w
mod <-
"
m ~ a * x + w + c1 + c2
y ~ b * m + x + d * mw + c1 + c2
w ~~ v_w * w
w ~ m_w * 1
ab := a * b
ab_lo := a * (b + d * (m_w - sqrt(v_w)))
ab_hi := a * (b + d * (m_w + sqrt(v_w)))
"
fit <- sem(mod, data_med_mod_b,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 5, 7, 10, 11, 30:32), ]
#>      lhs op                     rhs label   est    se      z pvalue ci.lower
#> 1      m  ~                       x     a 0.300 0.073  4.127  0.000    0.158
#> 5      y  ~                       m     b 0.841 0.475  1.770  0.077   -0.091
#> 7      y  ~                      mw     d 0.629 0.039 16.123  0.000    0.553
#> 10     w ~~                       w   v_w 0.960 0.136  7.071  0.000    0.694
#> 11     w ~1                           m_w 2.179 0.098 22.240  0.000    1.987
#> 30    ab :=                     a*b    ab 0.253 0.155  1.626  0.104   -0.052
#> 31 ab_lo := a*(b+d*(m_w-sqrt(v_w))) ab_lo 0.480 0.186  2.578  0.010    0.115
#> 32 ab_hi := a*(b+d*(m_w+sqrt(v_w))) ab_hi 0.850 0.254  3.342  0.001    0.352
#>    ci.upper
#> 1     0.443
#> 5     1.773
#> 7     0.706
#> 10    1.226
#> 11    2.371
#> 30    0.557
#> 31    0.844
#> 32    1.349
```
