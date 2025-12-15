# Sample Dataset: Serial-Parallel Moderated Mediation with Two Categorical Moderators

A serial-parallel mediation model with two categorical moderators.

## Usage

``` r
data_med_mod_serial_parallel_cat
```

## Format

A data frame with 300 rows and 8 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator. String. Values: "group1", "group2", "group3"

- w2:

  Moderator. String. Values: "team1", "team2"

- m11:

  Mediator 1 in Path 1. Numeric.

- m12:

  Mediator 2 in Path 1. Numeric.

- m2:

  Mediator in Path 2. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
data(data_med_mod_serial_parallel_cat)
dat <- data_med_mod_serial_parallel_cat
summary(lm_m11 <- lm(m11 ~ x*w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m11 ~ x * w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.0619 -0.6060 -0.0216  0.6404  3.4200 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.3209962  0.6435180   0.499  0.61829    
#> x            0.9510556  0.0940888  10.108  < 2e-16 ***
#> w1group2     0.9280457  0.8350532   1.111  0.26733    
#> w1group3     1.9216220  0.8739049   2.199  0.02867 *  
#> c1          -0.0009806  0.0545802  -0.018  0.98568    
#> c2          -0.0064057  0.0580329  -0.110  0.91218    
#> x:w1group2  -0.1964450  0.1338652  -1.467  0.14332    
#> x:w1group3  -0.4204409  0.1276517  -3.294  0.00111 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.955 on 292 degrees of freedom
#> Multiple R-squared:  0.4167, Adjusted R-squared:  0.4027 
#> F-statistic:  29.8 on 7 and 292 DF,  p-value: < 2.2e-16
#> 
summary(lm_m12 <- lm(m12 ~ m11 + x + w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m12 ~ m11 + x + w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.83583 -0.56576  0.02488  0.59982  2.07412 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  2.239944   0.480360   4.663 4.74e-06 ***
#> m11          0.042372   0.058007   0.730    0.466    
#> x            0.430485   0.068153   6.316 9.91e-10 ***
#> w1group2     0.112602   0.140247   0.803    0.423    
#> w1group3     0.051596   0.166553   0.310    0.757    
#> c1           0.054272   0.054916   0.988    0.324    
#> c2          -0.006007   0.058548  -0.103    0.918    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9642 on 293 degrees of freedom
#> Multiple R-squared:  0.2655, Adjusted R-squared:  0.2505 
#> F-statistic: 17.66 on 6 and 293 DF,  p-value: < 2.2e-16
#> 
summary(lm_m2 <- lm(m2 ~ x + w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m2 ~ x + w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.50745 -0.72556  0.01446  0.74006  2.64019 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  2.87424    0.50746   5.664 3.52e-08 ***
#> x            0.44820    0.05732   7.820 9.54e-14 ***
#> w1group2    -0.12412    0.15005  -0.827    0.409    
#> w1group3    -0.05783    0.16970  -0.341    0.733    
#> c1          -0.02093    0.05907  -0.354    0.723    
#> c2          -0.01997    0.06298  -0.317    0.751    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.037 on 294 degrees of freedom
#> Multiple R-squared:  0.2303, Adjusted R-squared:  0.2172 
#> F-statistic: 17.59 on 5 and 294 DF,  p-value: 3.033e-15
#> 
summary(lm_y <- lm(y ~ m12 + m2*w2 + m12 + x + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y ~ m12 + m2 * w2 + m12 + x + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.0979 -0.7856 -0.0818  0.7568  3.6255 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.462047   0.754233   0.613   0.5406    
#> m12          0.815192   0.074408  10.956  < 2e-16 ***
#> m2           0.448334   0.099012   4.528 8.68e-06 ***
#> w2team2      1.308023   0.704869   1.856   0.0645 .  
#> x            0.002084   0.073283   0.028   0.9773    
#> c1          -0.020047   0.069981  -0.286   0.7747    
#> c2          -0.022310   0.075188  -0.297   0.7669    
#> m2:w2team2  -0.228928   0.122836  -1.864   0.0634 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.226 on 292 degrees of freedom
#> Multiple R-squared:  0.4392, Adjusted R-squared:  0.4258 
#> F-statistic: 32.67 on 7 and 292 DF,  p-value: < 2.2e-16
#> 
```
