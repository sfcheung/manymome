# Sample Dataset: Parallel Mediation with Two Moderators

A parallel mediation model with a1-path and b2-path moderated.

## Usage

``` r
data_med_mod_parallel
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
data(data_med_mod_parallel)
data_med_mod_parallel$xw1 <-
 data_med_mod_parallel$x *
 data_med_mod_parallel$w1
data_med_mod_parallel$m2w2 <-
 data_med_mod_parallel$m2 *
 data_med_mod_parallel$w2
mod <-
"
m1 ~ a1 * x + w1 + da1 * xw1 + c1 + c2
m2 ~ a2 * x + w1 + c1 + c2
y ~ b1 * m1 + b2 * m2 + x + w1 + w2 + db2 * m2w2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
a1b1 := a1 * b1
a2b2 := a2 * b2
a1b1_w1lo := (a1 + da1 * (m_w1 - sqrt(v_w1))) * b1
a1b1_w1hi := (a1 + da1 * (m_w1 + sqrt(v_w1))) * b2
a2b2_w2lo := a2 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
a2b2_w2hi := a2 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
"
fit <- sem(mod, data_med_mod_parallel,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 10, 11, 15, 48:53), ]
#>          lhs op                           rhs     label    est    se      z
#> 1         m1  ~                             x        a1 -1.328 0.136 -9.741
#> 3         m1  ~                           xw1       da1  0.289 0.010 28.738
#> 6         m2  ~                             x        a2  0.184 0.068  2.704
#> 10         y  ~                            m1        b1  0.368 0.030 12.437
#> 11         y  ~                            m2        b2 -0.363 0.091 -3.966
#> 15         y  ~                          m2w2       db2  0.296 0.007 39.907
#> 48      a1b1 :=                         a1*b1      a1b1 -0.489 0.064 -7.669
#> 49      a2b2 :=                         a2*b2      a2b2 -0.067 0.030 -2.234
#> 50 a1b1_w1lo := (a1+da1*(m_w1-sqrt(v_w1)))*b1 a1b1_w1lo -0.075 0.042 -1.815
#> 51 a1b1_w1hi := (a1+da1*(m_w1+sqrt(v_w1)))*b2 a1b1_w1hi -0.155 0.053 -2.943
#> 52 a2b2_w2lo := a2*(b2+db2*(m_w2-sqrt(v_w2))) a2b2_w2lo  0.046 0.025  1.831
#> 53 a2b2_w2hi := a2*(b2+db2*(m_w2+sqrt(v_w2))) a2b2_w2hi  0.158 0.061  2.571
#>    pvalue ci.lower ci.upper
#> 1   0.000   -1.595   -1.061
#> 3   0.000    0.270    0.309
#> 6   0.007    0.051    0.318
#> 10  0.000    0.310    0.426
#> 11  0.000   -0.542   -0.183
#> 15  0.000    0.282    0.311
#> 48  0.000   -0.614   -0.364
#> 49  0.025   -0.126   -0.008
#> 50  0.069   -0.157    0.006
#> 51  0.003   -0.258   -0.052
#> 52  0.067   -0.003    0.095
#> 53  0.010    0.038    0.278
```
