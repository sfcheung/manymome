# Sample Dataset: A Latent Variable Mediation Model With 4 Factors

This data set is for testing functions in a four-factor structural
model.

## Usage

``` r
data_sem
```

## Format

A data frame with 200 rows and 14 variables:

- x01:

  Indicator. Numeric.

- x02:

  Indicator. Numeric.

- x03:

  Indicator. Numeric.

- x04:

  Indicator. Numeric.

- x05:

  Indicator. Numeric.

- x06:

  Indicator. Numeric.

- x07:

  Indicator. Numeric.

- x08:

  Indicator. Numeric.

- x09:

  Indicator. Numeric.

- x10:

  Indicator. Numeric.

- x11:

  Indicator. Numeric.

- x12:

  Indicator. Numeric.

- x13:

  Indicator. Numeric.

- x14:

  Indicator. Numeric.

## Examples

``` r
data(data_sem)
dat <- data_med_mod_b_mod
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  a1*f1 + a2*f2
   f4 ~  b1*f1 + b3*f3
   a1b3 := a1 * b3
   a2b3 := a2 * b3
  '
fit <- lavaan::sem(model = mod, data = data_sem)
summary(fit)
#> lavaan 0.6-20 ended normally after 42 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        33
#> 
#>   Number of observations                           200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                72.210
#>   Degrees of freedom                                72
#>   P-value (Chi-square)                           0.471
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 =~                                               
#>     x01               1.000                           
#>     x02               0.812    0.134    6.081    0.000
#>     x03               1.381    0.220    6.291    0.000
#>   f2 =~                                               
#>     x04               1.000                           
#>     x05               0.058    0.076    0.755    0.450
#>     x06               1.153    0.162    7.126    0.000
#>     x07               0.738    0.108    6.831    0.000
#>   f3 =~                                               
#>     x08               1.000                           
#>     x09               0.752    0.090    8.339    0.000
#>     x10               1.078    0.120    8.969    0.000
#>   f4 =~                                               
#>     x11               1.000                           
#>     x12               0.776    0.091    8.507    0.000
#>     x13               0.123    0.070    1.745    0.081
#>     x14               1.125    0.121    9.311    0.000
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f3 ~                                                
#>     f1        (a1)    0.243    0.120    2.018    0.044
#>     f2        (a2)    0.326    0.102    3.186    0.001
#>   f4 ~                                                
#>     f1        (b1)    0.447    0.125    3.592    0.000
#>     f3        (b3)    0.402    0.090    4.445    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   f1 ~~                                               
#>     f2               -0.039    0.085   -0.457    0.648
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .x01               1.064    0.151    7.025    0.000
#>    .x02               1.075    0.131    8.223    0.000
#>    .x03               0.860    0.217    3.958    0.000
#>    .x04               1.014    0.166    6.111    0.000
#>    .x05               0.966    0.097    9.989    0.000
#>    .x06               0.837    0.193    4.348    0.000
#>    .x07               1.065    0.130    8.222    0.000
#>    .x08               0.735    0.138    5.310    0.000
#>    .x09               0.941    0.118    7.984    0.000
#>    .x10               1.134    0.179    6.346    0.000
#>    .x11               1.016    0.155    6.571    0.000
#>    .x12               0.985    0.123    7.981    0.000
#>    .x13               1.095    0.110    9.957    0.000
#>    .x14               0.911    0.171    5.329    0.000
#>     f1                0.765    0.184    4.164    0.000
#>     f2                1.077    0.225    4.784    0.000
#>    .f3                1.175    0.207    5.675    0.000
#>    .f4                0.949    0.185    5.125    0.000
#> 
#> Defined Parameters:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     a1b3              0.098    0.051    1.918    0.055
#>     a2b3              0.131    0.049    2.676    0.007
#> 
```
