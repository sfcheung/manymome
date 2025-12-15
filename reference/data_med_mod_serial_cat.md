# Sample Dataset: Serial Moderated Mediation with Two Categorical Moderators

A serial mediation model with two categorical moderators.

## Usage

``` r
data_med_mod_serial_cat
```

## Format

A data frame with 300 rows and 8 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator. String. Values: "group1", "group2", "group3"

- w2:

  Moderator. String. Values: "team1", "team2"

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
data(data_med_mod_serial_cat)
dat <- data_med_mod_serial_cat
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m1 ~ x * w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.48162 -0.63917  0.03616  0.62807  2.88106 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.13552    0.66475  -0.204 0.838596    
#> x            0.97270    0.10335   9.411  < 2e-16 ***
#> w1group2    -0.23379    0.90635  -0.258 0.796628    
#> w1group3     2.53055    0.90458   2.797 0.005493 ** 
#> c1           0.06033    0.06185   0.975 0.330139    
#> c2           0.04888    0.05965   0.819 0.413195    
#> x:w1group2  -0.03211    0.14433  -0.222 0.824096    
#> x:w1group3  -0.51225    0.14585  -3.512 0.000515 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.047 on 292 degrees of freedom
#> Multiple R-squared:  0.4478, Adjusted R-squared:  0.4346 
#> F-statistic: 33.83 on 7 and 292 DF,  p-value: < 2.2e-16
#> 
summary(lm_m2 <- lm(m2 ~ m1 + x + w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m2 ~ m1 + x + w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -3.03503 -0.65833  0.02794  0.64110  2.99045 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.314339   0.443355   7.476 8.95e-13 ***
#> m1           0.363210   0.055052   6.598 1.95e-10 ***
#> x            0.005616   0.072660   0.077  0.93844    
#> w1group2    -0.415468   0.148938  -2.790  0.00562 ** 
#> w1group3    -0.075908   0.145738  -0.521  0.60286    
#> c1          -0.034240   0.059456  -0.576  0.56513    
#> c2           0.010818   0.057533   0.188  0.85098    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.01 on 293 degrees of freedom
#> Multiple R-squared:  0.2334, Adjusted R-squared:  0.2177 
#> F-statistic: 14.87 on 6 and 293 DF,  p-value: 7.661e-15
#> 
summary(lm_y <- lm(y ~ m2*w2 + m1 + x + w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y ~ m2 * w2 + m1 + x + w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.30328 -0.69259  0.03058  0.67952  2.89379 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  1.81161    0.56116   3.228  0.00139 ** 
#> m2           0.39472    0.07703   5.124 5.46e-07 ***
#> w2team2      0.48363    0.56320   0.859  0.39120    
#> m1           0.11568    0.05984   1.933  0.05418 .  
#> x           -0.06053    0.07369  -0.821  0.41210    
#> w1group2    -0.02577    0.15383  -0.168  0.86706    
#> w1group3    -0.14960    0.14819  -1.010  0.31358    
#> c1          -0.08765    0.06035  -1.452  0.14750    
#> c2           0.08223    0.05853   1.405  0.16112    
#> m2:w2team2  -0.07549    0.10503  -0.719  0.47288    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.024 on 290 degrees of freedom
#> Multiple R-squared:  0.2074, Adjusted R-squared:  0.1828 
#> F-statistic:  8.43 on 9 and 290 DF,  p-value: 3.442e-11
#> 
```
