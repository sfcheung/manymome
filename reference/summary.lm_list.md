# Summary of an `lm_list`-Class Object

The summary of content of the output of
[`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

## Usage

``` r
# S3 method for class 'lm_list'
summary(object, betaselect = FALSE, ci = FALSE, level = 0.95, ...)

# S3 method for class 'summary_lm_list'
print(x, digits = 3, digits_decimal = NULL, ...)
```

## Arguments

- object:

  The output of
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

- betaselect:

  If `TRUE`, standardized coefficients are computed and included in the
  printout. Only numeric variables will be computed, and any derived
  terms, such as product terms, will be formed *after* standardization.
  Default is `FALSE`.

- ci:

  If `TRUE`, confidence interval based on *t* statistic and standard
  error will be computed and added to the output. Default is `FALSE`.

- level:

  The level of confidence of the confidence interval. Ignored if `ci` is
  not `TRUE`.

- ...:

  Other arguments. Not used.

- x:

  An object of class `summary_lm_list`.

- digits:

  The number of significant digits in printing numerical results.

- digits_decimal:

  The number of digits after the decimal in printing numerical results.
  Default is `NULL`. If set to an integer, numerical results in the
  coefficient table will be printed according this setting, and `digits`
  will be ignored.

## Value

`summary.lm_list()` returns a `summary_lm_list`-class object, which is a
list of the [`summary()`](https://rdrr.io/r/base/summary.html) outputs
of the [`lm()`](https://rdrr.io/r/stats/lm.html) outputs stored.

`print.summary_lm_list()` returns `x` invisibly. Called for its side
effect.

## Functions

- `print(summary_lm_list)`: Print method for output of summary for
  lm_list.

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
```
