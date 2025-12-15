# Sample Dataset: A Simple Mediation Model with b-Path Moderated-Moderation

A simple mediation model with moderated-mediation on the b-path.

## Usage

``` r
data_med_mod_b_mod
```

## Format

A data frame with 100 rows and 5 variables:

- x:

  Predictor. Numeric.

- w1:

  Moderator on b-path. Numeric.

- w2:

  Moderator on the moderating effect of w1. Numeric.

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
data(data_med_mod_b_mod)
dat <- data_med_mod_b_mod
summary(lm_m <- lm(m ~ x + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m ~ x + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.20492 -0.61466 -0.00616  0.66926  2.99114 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -1.28350    1.41914  -0.904    0.368    
#> x            0.48501    0.09547   5.080 1.85e-06 ***
#> c1           0.03757    0.08769   0.428    0.669    
#> c2           0.02441    0.08581   0.285    0.777    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9498 on 96 degrees of freedom
#> Multiple R-squared:  0.2123, Adjusted R-squared:  0.1877 
#> F-statistic: 8.625 on 3 and 96 DF,  p-value: 3.987e-05
#> 
summary(lm_y <- lm(y ~ m*w1*w2 + x + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y ~ m * w1 * w2 + x + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.18204 -0.47099 -0.02998  0.53605  2.77463 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -17.40513    6.18440  -2.814  0.00602 ** 
#> m             6.47229    1.54231   4.196 6.40e-05 ***
#> w1            5.06717    1.20737   4.197 6.39e-05 ***
#> w2            8.08655    1.97632   4.092 9.39e-05 ***
#> x             0.08443    0.10955   0.771  0.44293    
#> c1           -0.09960    0.08419  -1.183  0.23995    
#> c2           -0.14547    0.08432  -1.725  0.08798 .  
#> m:w1         -1.23202    0.30195  -4.080 9.79e-05 ***
#> m:w2         -1.98779    0.47670  -4.170 7.06e-05 ***
#> w1:w2        -1.63594    0.39291  -4.164 7.22e-05 ***
#> m:w1:w2       0.39674    0.09034   4.391 3.09e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9022 on 89 degrees of freedom
#> Multiple R-squared:  0.4306, Adjusted R-squared:  0.3667 
#> F-statistic: 6.732 on 10 and 89 DF,  p-value: 1.025e-07
#> 
```
