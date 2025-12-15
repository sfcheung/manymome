# Extract the Indirect Effect or Conditional Indirect Effect

Return the estimate of the indirect effect in the output of
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
or or the conditional indirect in the output of
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'indirect'
coef(object, ...)
```

## Arguments

- object:

  The output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  or
  [`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- ...:

  Optional arguments. Ignored by the function.

## Value

A scalar: The estimate of the indirect effect or conditional indirect
effect.

## Details

It extracts and returns the element `indirect`. in an object.

If standardized effect is requested when calling
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
or
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
the effect returned is also standardized.

## See also

[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x + w1 + x:w1
m2 ~ x
y  ~ m1 + m2 + x
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Examples for indirect_effect():

# Inidrect effect from x through m2 to y
out1 <- indirect_effect(x = "x", y = "y", m = "m2", fit = fit)
out1
#> 
#> == Indirect Effect  ==
#>                               
#>  Path:            x -> m2 -> y
#>  Indirect Effect: 0.052       
#> 
#> Computation Formula:
#>   (b.m2~x)*(b.y~m2)
#> 
#> Computation:
#>   (0.16554)*(0.31451)
#> 
#> Coefficients of Component Paths:
#>  Path Coefficient
#>  m2~x       0.166
#>  y~m2       0.315
#> 
coef(out1)
#>        y~x 
#> 0.05206422 

# Conditional Indirect effect from x1 through m1 to y,
# when w1 is 1 SD above mean
hi_w1 <- mean(dat$w1) + sd(dat$w1)
out2 <- cond_indirect(x = "x", y = "y", m = "m1",
                      wvalues = c(w1 = hi_w1), fit = fit)
out2
#> 
#> == Conditional Indirect Effect   ==
#>                                           
#>  Path:                        x -> m1 -> y
#>  Moderators:                  w1          
#>  Conditional Indirect Effect: -0.031      
#>  When:                        w1 = 1.228  
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.y~m1)
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(1.22806))*(-0.04197)
#> 
#> Coefficients of Component Paths:
#>  Path Conditional Effect Original Coefficient
#>  m1~x              0.750                0.463
#>  y~m1             -0.042               -0.042
#> 
coef(out2)
#>         y~x 
#> -0.03147387 
```
