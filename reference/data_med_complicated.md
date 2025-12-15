# Sample Dataset: A Complicated Mediation Model

A mediation model with two predictors, two pathways,

## Usage

``` r
data_med_complicated
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

## Examples

``` r
data(data_med_complicated)
dat <- data_med_complicated
summary(lm_m11 <- lm(m11 ~ x1 + x1 + x2 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m11 ~ x1 + x1 + x2 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.64294 -0.55585  0.08916  0.67243  1.70807 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  2.13910    1.32319   1.617 0.109276    
#> x1           0.35204    0.08946   3.935 0.000158 ***
#> x2          -0.04471    0.09540  -0.469 0.640340    
#> c1           0.07961    0.10081   0.790 0.431666    
#> c2          -0.09890    0.08790  -1.125 0.263356    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9613 on 95 degrees of freedom
#> Multiple R-squared:  0.1679, Adjusted R-squared:  0.1328 
#> F-statistic: 4.791 on 4 and 95 DF,  p-value: 0.001453
#> 
summary(lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m12 ~ m11 + x1 + x2 + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.4487 -0.6344  0.1066  0.5949  2.4585 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  8.23329    1.38073   5.963 4.32e-08 ***
#> m11          0.45408    0.10562   4.299 4.18e-05 ***
#> x1          -0.21182    0.09931  -2.133   0.0355 *  
#> x2          -0.07206    0.09832  -0.733   0.4654    
#> c1           0.20391    0.10412   1.959   0.0531 .  
#> c2          -0.42456    0.09109  -4.661 1.04e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9896 on 94 degrees of freedom
#> Multiple R-squared:  0.3525, Adjusted R-squared:  0.318 
#> F-statistic: 10.23 on 5 and 94 DF,  p-value: 7.515e-08
#> 
summary(lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = m2 ~ x1 + x2 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.25156 -0.62962 -0.02606  0.64273  1.67002 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  7.19926    1.21769   5.912 5.28e-08 ***
#> x1           0.02233    0.08232   0.271   0.7868    
#> x2           0.28901    0.08779   3.292   0.0014 ** 
#> c1          -0.13437    0.09277  -1.448   0.1508    
#> c2          -0.01723    0.08089  -0.213   0.8318    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.8847 on 95 degrees of freedom
#> Multiple R-squared:  0.1167, Adjusted R-squared:  0.07948 
#> F-statistic: 3.137 on 4 and 95 DF,  p-value: 0.01805
#> 
summary(lm_y1 <- lm(y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.52098 -0.63677 -0.04699  0.60540  2.05599 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  7.44629    1.72403   4.319 3.95e-05 ***
#> m11          0.14694    0.10966   1.340  0.18354    
#> m12          0.23402    0.09787   2.391  0.01884 *  
#> m2          -0.43300    0.10894  -3.975  0.00014 ***
#> x1          -0.07840    0.09651  -0.812  0.41865    
#> x2           0.00322    0.09875   0.033  0.97406    
#> c1           0.04238    0.10188   0.416  0.67842    
#> c2          -0.04145    0.09592  -0.432  0.66666    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9391 on 92 degrees of freedom
#> Multiple R-squared:  0.2786, Adjusted R-squared:  0.2237 
#> F-statistic: 5.075 on 7 and 92 DF,  p-value: 6.688e-05
#> 
summary(lm_y2 <- lm(y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat))
#> 
#> Call:
#> lm(formula = y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2.4153 -0.5711  0.1328  0.6137  2.2840 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 10.98160    1.95311   5.623    2e-07 ***
#> m11          0.02417    0.12423   0.195 0.846185    
#> m12          0.13525    0.11088   1.220 0.225649    
#> m2          -0.43598    0.12341  -3.533 0.000645 ***
#> x1           0.11534    0.10933   1.055 0.294202    
#> x2           0.06204    0.11187   0.555 0.580515    
#> c1          -0.04905    0.11542  -0.425 0.671820    
#> c2           0.04581    0.10867   0.422 0.674297    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.064 on 92 degrees of freedom
#> Multiple R-squared:  0.1526, Adjusted R-squared:  0.08818 
#> F-statistic: 2.368 on 7 and 92 DF,  p-value: 0.02857
#> 
```
