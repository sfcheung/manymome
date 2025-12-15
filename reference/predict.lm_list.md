# Predicted Values of an 'lm_list'-Class Object

Compute the predicted values based on the models stored in an
'lm_list\`-class object.

## Usage

``` r
# S3 method for class 'lm_list'
predict(object, x = NULL, y = NULL, m = NULL, newdata, ...)
```

## Arguments

- object:

  An 'lm_list'-class object.

- x:

  The variable name at the start of a path.

- y:

  The variable name at the end of a path.

- m:

  Optional. The mediator(s) from `x` to `y`. A numeric vector of the
  names of the mediators. The path goes from the first element to the
  last element. For example, if `m = c("m1", "m2")`, then the path is
  `x -> m1 -> m2 -> y`.

- newdata:

  Required. A data frame of the new data. It must be a data frame.

- ...:

  Additional arguments. Ignored.

## Value

A numeric vector of the predicted values, with length equal to the
number of rows of user-supplied data.

## Details

An `lm_list`-class object is a list of `lm`-class objects, this function
is similar to the
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) method of
[`lm()`](https://rdrr.io/r/stats/lm.html) but it works on a system
defined by a list of regression models.

This is an advanced helper used by some functions in this package.
Exported for advanced users.

## See also

[`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md)

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
newdat <- data_serial_parallel[3:5, ]
predict(lm_serial_parallel,
        x = "x",
        y = "y",
        m = "m2",
        newdata = newdat)
#> [1] 16.24671 19.60951 16.58806
```
