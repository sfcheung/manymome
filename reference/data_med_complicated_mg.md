# Sample Dataset: A Complicated Mediation Model With Two Groups

A mediation model with two predictors, two pathways, and two groups.

## Usage

``` r
data_med_complicated_mg
```

## Format

A data frame with 300 rows and 5 variables:

- x1:

  Predictor 1. Numeric.

- x2:

  Predictor 2. Numeric.

- m11:

  Mediator 1 in Path 1. Numeric.

- m12:

  Mediator 2 in Path 1. Numeric.

- m2:

  Mediator in Path 2. Numeric.

- y1:

  Outcome variable 1. Numeric.

- y2:

  Outcome variable 2. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

- group:

  Group variable. Character. 'Group A' or 'Group B'

## Examples

``` r
library(lavaan)
data(data_med_complicated_mg)
dat <- data_med_complicated_mg
mod <-
"
m11 ~ x1 + x2 + c1 + c2
m12 ~ m11 + c1 + c2
m2 ~ x1 + x2 + c1 + c2
y1 ~ m11 + m12 + x1 + x2 + c1 + c2
y2 ~ m2 + x1 + x2 + c1 + c2
"
fit <- sem(mod, dat, group = "group")
summary(fit)
#> lavaan 0.6-21 ended normally after 13 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        66
#> 
#>   Number of observations per group:                   
#>     Group A                                        100
#>     Group B                                        100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                16.359
#>   Degrees of freedom                                14
#>   P-value (Chi-square)                           0.292
#>   Test statistic for each group:
#>     Group A                                      7.443
#>     Group B                                      8.917
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> 
#> Group 1 [Group A]:
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   m11 ~                                               
#>     x1                0.360    0.089    4.037    0.000
#>     x2                0.222    0.103    2.157    0.031
#>     c1                0.275    0.091    3.005    0.003
#>     c2                0.114    0.092    1.240    0.215
#>   m12 ~                                               
#>     m11               0.593    0.088    6.698    0.000
#>     c1                0.030    0.091    0.327    0.743
#>     c2               -0.178    0.089   -1.998    0.046
#>   m2 ~                                                
#>     x1                0.005    0.102    0.045    0.964
#>     x2                0.542    0.117    4.626    0.000
#>     c1                0.082    0.104    0.791    0.429
#>     c2                0.208    0.104    1.992    0.046
#>   y1 ~                                                
#>     m11               0.372    0.119    3.116    0.002
#>     m12               0.351    0.105    3.342    0.001
#>     x1               -0.099    0.098   -1.011    0.312
#>     x2               -0.067    0.107   -0.629    0.529
#>     c1               -0.056    0.097   -0.572    0.567
#>     c2               -0.149    0.096   -1.554    0.120
#>   y2 ~                                                
#>     m2                0.395    0.083    4.771    0.000
#>     x1                0.105    0.084    1.249    0.212
#>     x2                0.178    0.107    1.666    0.096
#>     c1               -0.076    0.087   -0.874    0.382
#>     c2                0.080    0.088    0.912    0.362
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y2               -0.031    0.084   -0.368    0.713
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               0.084    0.096    0.874    0.382
#>    .m12              -0.042    0.093   -0.452    0.651
#>    .m2                0.013    0.109    0.116    0.907
#>    .y1                0.011    0.098    0.108    0.914
#>    .y2               -0.108    0.090   -1.191    0.234
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               0.868    0.123    7.071    0.000
#>    .m12               0.820    0.116    7.071    0.000
#>    .m2                1.126    0.159    7.071    0.000
#>    .y1                0.904    0.128    7.071    0.000
#>    .y2                0.774    0.109    7.071    0.000
#> 
#> 
#> Group 2 [Group B]:
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   m11 ~                                               
#>     x1                0.104    0.107    0.970    0.332
#>     x2               -0.012    0.106   -0.114    0.909
#>     c1                0.364    0.103    3.526    0.000
#>     c2                0.106    0.109    0.978    0.328
#>   m12 ~                                               
#>     m11               0.346    0.096    3.606    0.000
#>     c1                0.219    0.102    2.136    0.033
#>     c2               -0.135    0.105   -1.285    0.199
#>   m2 ~                                                
#>     x1               -0.057    0.104   -0.542    0.588
#>     x2                0.307    0.103    2.965    0.003
#>     c1                0.223    0.101    2.218    0.027
#>     c2                0.181    0.106    1.700    0.089
#>   y1 ~                                                
#>     m11               0.351    0.100    3.509    0.000
#>     m12               0.056    0.098    0.568    0.570
#>     x1                0.080    0.102    0.781    0.435
#>     x2                0.016    0.100    0.157    0.875
#>     c1               -0.294    0.106   -2.782    0.005
#>     c2                0.061    0.104    0.582    0.561
#>   y2 ~                                                
#>     m2                0.398    0.099    4.025    0.000
#>     x1                0.023    0.104    0.224    0.823
#>     x2                0.301    0.107    2.812    0.005
#>     c1                0.110    0.102    1.076    0.282
#>     c2               -0.008    0.107   -0.076    0.940
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y2               -0.077    0.096   -0.805    0.421
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               0.112    0.104    1.075    0.282
#>    .m12               0.149    0.100    1.478    0.139
#>    .m2                0.112    0.101    1.107    0.268
#>    .y1                0.057    0.100    0.575    0.565
#>    .y2                0.198    0.101    1.959    0.050
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               1.044    0.148    7.071    0.000
#>    .m12               0.969    0.137    7.071    0.000
#>    .m2                0.992    0.140    7.071    0.000
#>    .y1                0.933    0.132    7.071    0.000
#>    .y2                0.978    0.138    7.071    0.000
#> 
```
