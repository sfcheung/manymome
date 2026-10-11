# Sample Dataset: A Latent Variable Moderated Mediation Model With 4 Factors

This data set is for testing functions for moderated mediation among
latent variables.

## Usage

``` r
data_sem_mome
```

## Format

A data frame with 500 rows and 16 variables:

- x1:

  Indicator. Numeric.

- x2:

  Indicator. Numeric.

- x3:

  Indicator. Numeric.

- x4:

  Indicator. Numeric.

- w1:

  Indicator. Numeric.

- w2:

  Indicator. Numeric.

- w3:

  Indicator. Numeric.

- w4:

  Indicator. Numeric.

- m1:

  Indicator. Numeric.

- m2:

  Indicator. Numeric.

- m3:

  Indicator. Numeric.

- m4:

  Indicator. Numeric.

- y1:

  Indicator. Numeric.

- y2:

  Indicator. Numeric.

- y3:

  Indicator. Numeric.

- y4:

  Indicator. Numeric.

## Examples

``` r
data(data_sem_mome)
mod <-
"
fx =~ x1 + x2 + x3 + x4
fw =~ w1 + w2 + w3 + w4
fm =~ m1 + m2 + m3 + m4
fy =~ y1 + y2 + y3 + y4
fm ~ fx + fw + fx:fw
fy ~ fm + fx
"
library(lavaan)
fit <- sam(model = mod, data = data_sem_mome)
summary(fit)
#> This is lavaan 0.7-3 -- using the SAM approach to SEM
#> 
#>   SAM method                                     LOCAL
#>   Mapping matrix M method                           ML
#>   Number of measurement blocks                       4
#>   Estimator measurement part                        ML
#>   Estimator  structural part                        ML
#> 
#>   Number of observations                           500
#> 
#> Summary Information Measurement + Structural:
#> 
#>   Block Latent Nind Chisq Df
#>       1     fm    4 1.912  2
#>       2     fw    4 3.416  2
#>       3     fx    4 1.782  2
#>       4     fy    4 2.332  2
#> 
#>   Model-based reliability latent variables:
#> 
#>      fx    fw    fm    fy
#>   0.887 0.881 0.706 0.831
#> 
#>   Note: the lambda truncation engaged (lambda.star = 0.935):
#>   the structural estimates are biased (typically attenuated), and large
#>   coefficients may have undercovering confidence intervals.
#>     max. approximate shrinkage bias : 0.020 (at most 0.18 SE)
#> 
#>   Summary Information Structural part:
#> 
#>   chisq.scaled df.scaled pvalue.scaled cfi.robust rmsea.robust  srmr
#>          4.329         2         0.115       0.99        0.118 0.017
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                                Local
#>   Information                                 Expected
#>   Information bread                           Observed
#>   Information saturated (h1) model          Structured
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   fm ~                                                
#>     fx                0.398    0.040    9.890    0.000
#>     fw               -0.009    0.028   -0.329    0.743
#>     fx:fw             0.481    0.049    9.892    0.000
#>   fy ~                                                
#>     fm                0.425    0.128    3.317    0.001
#>     fx                0.037    0.068    0.543    0.587
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   fx ~~                                               
#>     fw               -0.016    0.034   -0.477    0.633
#>     fx:fw            -0.005    0.048   -0.112    0.911
#>   fw ~~                                               
#>     fx:fw             0.036    0.044    0.831    0.406
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     fx:fw            -0.016    0.034   -0.477    0.633
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     fx                0.648    0.065    9.967    0.000
#>     fw                0.701    0.066   10.553    0.000
#>    .fm                0.016    0.003    5.275    0.000
#>    .fy                0.402    0.043    9.288    0.000
#>     fx:fw             0.444    0.090    4.953    0.000
#> 
```
