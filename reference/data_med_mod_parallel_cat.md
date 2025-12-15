# Sample Dataset: Parallel Moderated Mediation with Two Categorical Moderators

A parallel mediation model with two categorical moderators.

## Usage

``` r
data_med_mod_parallel_cat
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
data(data_med_mod_parallel_cat)
dat <- data_med_mod_parallel_cat
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m1 ~ x * w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.7472 -0.6656  0.0500  0.6887  3.4973 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  1.143612   0.692888   1.651   0.0999 .  
#> x            0.902275   0.087667  10.292   <2e-16 ***
#> w1group2     1.150056   0.800215   1.437   0.1517    
#> w1group3     0.708515   0.846560   0.837   0.4033    
#> c1          -0.002886   0.062350  -0.046   0.9631    
#> c2          -0.127222   0.059342  -2.144   0.0329 *  
#> x:w1group2  -0.233639   0.117285  -1.992   0.0473 *  
#> x:w1group3  -0.238876   0.123572  -1.933   0.0542 .  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.025 on 292 degrees of freedom
#> Multiple R-squared:   0.49,  Adjusted R-squared:  0.4777 
#> F-statistic: 40.07 on 7 and 292 DF,  p-value: < 2.2e-16
#> 
summary(lm_m2 <- lm(m2 ~ x*w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m2 ~ x * w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.7490 -0.5931 -0.0192  0.6492  3.2015 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.224453   0.648430   4.973 1.13e-06 ***
#> x            0.438166   0.082042   5.341 1.86e-07 ***
#> w1group2     0.190078   0.748870   0.254   0.7998    
#> w1group3    -0.150416   0.792242  -0.190   0.8496    
#> c1          -0.129800   0.058349  -2.225   0.0269 *  
#> c2           0.007616   0.055535   0.137   0.8910    
#> x:w1group2  -0.053156   0.109759  -0.484   0.6285    
#> x:w1group3   0.025153   0.115643   0.218   0.8280    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9597 on 292 degrees of freedom
#> Multiple R-squared:  0.2529, Adjusted R-squared:  0.235 
#> F-statistic: 14.12 on 7 and 292 DF,  p-value: 8.862e-16
#> 
summary(lm_y <- lm(y ~ m1*w2 + m2*w2 + m1 + x + w1 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y ~ m1 * w2 + m2 * w2 + m1 + x + w1 + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.9753 -0.9732 -0.0471  0.9780  4.0917 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -0.335755   0.823638  -0.408 0.683833    
#> m1           0.897601   0.094224   9.526  < 2e-16 ***
#> w2team2      0.683583   0.919420   0.743 0.457787    
#> m2           0.426387   0.115277   3.699 0.000259 ***
#> x            0.044777   0.088262   0.507 0.612315    
#> w1group2    -0.194036   0.189154  -1.026 0.305839    
#> w1group3    -0.031134   0.204115  -0.153 0.878874    
#> c1          -0.125317   0.083119  -1.508 0.132728    
#> c2           0.083940   0.078548   1.069 0.286119    
#> m1:w2team2   0.006921   0.120168   0.058 0.954113    
#> w2team2:m2  -0.080865   0.159015  -0.509 0.611463    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.338 on 289 degrees of freedom
#> Multiple R-squared:  0.5841, Adjusted R-squared:  0.5697 
#> F-statistic: 40.59 on 10 and 289 DF,  p-value: < 2.2e-16
#> 
```
