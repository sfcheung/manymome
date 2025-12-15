# Sample Dataset: Simple Mediation with a-Path Moderated

A simple mediation model with a-path moderated.

## Usage

``` r
data_med_mod_a
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
data(data_med_mod_a)
data_med_mod_a$xw <-
 data_med_mod_a$x *
 data_med_mod_a$w
mod <-
"
m ~ a * x + w + d * xw + c1 + c2
y ~ b * m + x + w + c1 + c2
w ~~ v_w * w
w ~ m_w * 1
ab := a * b
ab_lo := (a + d * (m_w - sqrt(v_w))) * b
ab_hi := (a + d * (m_w + sqrt(v_w))) * b
"
fit <- sem(mod, data_med_mod_a,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 11, 12, 31:33), ]
#>      lhs op                     rhs label   est    se      z pvalue ci.lower
#> 1      m  ~                       x     a 0.096 0.455  0.211  0.833   -0.796
#> 3      m  ~                      xw     d 0.978 0.040 24.656  0.000    0.901
#> 6      y  ~                       m     b 0.923 0.040 23.114  0.000    0.845
#> 11     w ~~                       w   v_w 0.960 0.136  7.071  0.000    0.694
#> 12     w ~1                           m_w 2.179 0.098 22.239  0.000    1.987
#> 31    ab :=                     a*b    ab 0.089 0.420  0.211  0.833   -0.735
#> 32 ab_lo := (a+d*(m_w-sqrt(v_w)))*b ab_lo 1.172 0.427  2.742  0.006    0.334
#> 33 ab_hi := (a+d*(m_w+sqrt(v_w)))*b ab_hi 2.942 0.438  6.723  0.000    2.085
#>    ci.upper
#> 1     0.988
#> 3     1.056
#> 6     1.001
#> 11    1.226
#> 12    2.371
#> 31    0.912
#> 32    2.010
#> 33    3.800
```
