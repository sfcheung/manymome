# Join 'lm()' Output to Form an 'lm_list\`-Class Object

The resulting model can be used by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
or
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
as a path method, as if fitted by
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

## Usage

``` r
lm2list(...)
```

## Arguments

- ...:

  The [`lm()`](https://rdrr.io/r/stats/lm.html) outputs to be grouped in
  a list.

## Value

It returns an `lm_list`-class object that forms a path model represented
by a set of regression models. This class has a `summary` method that
shows the summary of each regression model stored (see
[`summary.lm_list()`](https://sfcheung.github.io/manymome/reference/summary.lm_list.md)),
and a `print` method that prints the models stored (see
[`print.lm_list()`](https://sfcheung.github.io/manymome/reference/print.lm_list.md)).

## Details

If a path model with mediation and/or moderation is fitted by a set of
regression models using [`lm()`](https://rdrr.io/r/stats/lm.html), this
function can combine them to an object of the class `lm_list` that
represents a path model, as one fitted by structural equation model
functions such as
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html). This class
of object can be used by some functions, such as
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
as if they were the output of fitting a path model, with the regression
coefficients treated as path coefficients.

The regression outputs to be combined need to meet the following
requirements:

- All models must be connected to at least one another model. That is, a
  regression model must either have (a) at least on predictor that is
  the outcome variable of another model, or (b) its outcome variable the
  predictor of another model.

- All models must be fitted to the same sample. This implies that the
  sample size must be the same in all analysis.

## See also

[`summary.lm_list()`](https://sfcheung.github.io/manymome/reference/summary.lm_list.md)
and
[`print.lm_list()`](https://sfcheung.github.io/manymome/reference/print.lm_list.md)
for related methods,
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
which accept `lm_list`-class objects as input.

## Examples

``` r


data(data_serial_parallel)
lm_m11 <- lm(m11 ~ x + c1 + c2, data_serial_parallel)
lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, data_serial_parallel)
lm_m2 <- lm(m2 ~ x + c1 + c2, data_serial_parallel)
lm_y <- lm(y ~ m11 + m12 + m2 + x + c1 + c2, data_serial_parallel)
# Join them to form a lm_list-class object
lm_serial_parallel <- lm2list(lm_m11, lm_m12, lm_m2, lm_y)
lm_serial_parallel
#> 
#> The model(s):
#> m11 ~ x + c1 + c2
#> m12 ~ m11 + x + c1 + c2
#> m2 ~ x + c1 + c2
#> y ~ m11 + m12 + m2 + x + c1 + c2
#> 
summary(lm_serial_parallel)
#> 
#> 
#> Model:
#> m11 ~ x + c1 + c2
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  11.4546     1.1258   10.17  < 2e-16 ***
#> x             0.8001     0.0953    8.39  4.2e-13 ***
#> c1            0.0855     0.1020    0.84    0.404    
#> c2           -0.2444     0.1002   -2.44    0.017 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.459. Adjusted R-square = 0.442. F(3, 96) = 27.148, p < .001
#> 
#> Model:
#> m12 ~ m11 + x + c1 + c2
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   9.8742     1.5048    6.56  2.8e-09 ***
#> m11           0.4652     0.0946    4.92  3.7e-06 ***
#> x             0.1146     0.1164    0.98   0.3274    
#> c1            0.1934     0.0949    2.04   0.0444 *  
#> c2           -0.2848     0.0957   -2.97   0.0037 ** 
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.469. Adjusted R-square = 0.446. F(4, 95) = 20.963, p < .001
#> 
#> Model:
#> m2 ~ x + c1 + c2
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)    2.354      1.236    1.91     0.06 .  
#> x              0.435      0.105    4.15  7.1e-05 ***
#> c1             0.178      0.112    1.59     0.12    
#> c2            -0.167      0.110   -1.52     0.13    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.196. Adjusted R-square = 0.171. F(3, 96) = 7.812, p < .001
#> 
#> Model:
#> y ~ m11 + m12 + m2 + x + c1 + c2
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) -1.791908   4.613263   -0.39  0.69859    
#> m11          0.203249   0.266930    0.76  0.44832    
#> m12          0.519112   0.255389    2.03  0.04494 *  
#> m2           0.838632   0.217639    3.85  0.00021 ***
#> x            0.071421   0.317264    0.23  0.82238    
#> c1          -0.000114   0.244934    0.00  0.99963    
#> c2          -0.069787   0.253231   -0.28  0.78348    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> R-square = 0.315. Adjusted R-square = 0.271. F(6, 93) = 7.133, p < .001

# Compute indirect effect from x to y through m11 and m12
outm11m12 <- cond_indirect(x = "x", y = "y",
                           m = c("m11", "m12"),
                           fit = lm_serial_parallel)
outm11m12
#> 
#> == Indirect Effect  ==
#>                                       
#>  Path:            x -> m11 -> m12 -> y
#>  Indirect Effect: 0.193               
#> 
#> Computation Formula:
#>   (b.m11~x)*(b.m12~m11)*(b.y~m12)
#> 
#> Computation:
#>   (0.80006)*(0.46521)*(0.51911)
#> 
#> Coefficients of Component Paths:
#>     Path Coefficient
#>    m11~x       0.800
#>  m12~m11       0.465
#>    y~m12       0.519
#> 
# Compute indirect effect from x to y
# through m11 and m12 with bootstrapping CI
# R should be at least 2000 or even 5000 in read study.
# In real research, parallel and progress can be omitted.
# They are est to TRUE by default.
outm11m12 <- cond_indirect(x = "x", y = "y",
                           m = c("m11", "m12"),
                           fit = lm_serial_parallel,
                           boot_ci = TRUE,
                           R = 100,
                           seed = 1234,
                           parallel = FALSE,
                           progress = FALSE)
outm11m12
#> 
#> == Indirect Effect  ==
#>                                          
#>  Path:               x -> m11 -> m12 -> y
#>  Indirect Effect:    0.193               
#>  95.0% Bootstrap CI: [-0.110 to 0.487]   
#> 
#> Computation Formula:
#>   (b.m11~x)*(b.m12~m11)*(b.y~m12)
#> 
#> Computation:
#>   (0.80006)*(0.46521)*(0.51911)
#> 
#> 
#> Percentile confidence interval formed by nonparametric bootstrapping
#> with 100 bootstrap samples.
#> 
#> Coefficients of Component Paths:
#>     Path Coefficient
#>    m11~x       0.800
#>  m12~m11       0.465
#>    y~m12       0.519
#> 
```
