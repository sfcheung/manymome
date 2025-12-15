# Sample Dataset: Moderation with One Categorical Moderator

A moderation model with a categorical moderator.

## Usage

``` r
data_mod_cat
```

## Format

A data frame with 300 rows and 5 variables:

- x:

  Predictor. Numeric.

- w:

  Moderator. String. Values: "group1", "group2", "group3"

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
data(data_mod_cat)
dat <- data_mod_cat
summary(lm_y <- lm(y ~ x*w + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y ~ x * w + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.51583 -0.64354 -0.00324  0.62382  3.08839 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.740541   0.521689   7.170 6.17e-12 ***
#> x            0.811466   0.087904   9.231  < 2e-16 ***
#> wgroup2     -0.127138   0.458910  -0.277  0.78194    
#> wgroup3      0.325005   0.415632   0.782  0.43487    
#> c1          -0.003431   0.056895  -0.060  0.95196    
#> c2          -0.059157   0.056892  -1.040  0.29929    
#> x:wgroup2   -0.126358   0.140534  -0.899  0.36933    
#> x:wgroup3   -0.376700   0.128669  -2.928  0.00368 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9806 on 292 degrees of freedom
#> Multiple R-squared:  0.3699, Adjusted R-squared:  0.3548 
#> F-statistic: 24.49 on 7 and 292 DF,  p-value: < 2.2e-16
#> 
```
