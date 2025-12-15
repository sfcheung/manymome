# Estimates of Conditional Indirect Effects or Conditional Effects

Return the estimates of the conditional indirect effects or conditional
effects for all levels in the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'cond_indirect_effects'
coef(object, ...)
```

## Arguments

- object:

  The output of
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- ...:

  Optional arguments. Ignored by the function.

## Value

A numeric vector: The estimates of the conditional effects or
conditional indirect effects.

## Details

It extracts and returns the column `ind` or `std` in the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## See also

[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x  + w1 + x:w1
m2 ~ m1
y  ~ m2 + x + w4 + m2:w4
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Conditional effects from x to m1 when w1 is equal to each of the levels
out1 <- cond_indirect_effects(x = "x", y = "m1",
                      wlevels = c("w1"), fit = fit)
out1
#> 
#> == Conditional effects ==
#> 
#>  Path: x -> m1
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>      [w1]   (w1)   ind
#> 1 M+1.0SD  1.228 0.750
#> 2 Mean     0.259 0.523
#> 3 M-1.0SD -0.710 0.297
#> 
#>  - The 'ind' column shows the conditional effects.
#>  
coef(out1)
#> w1: M+1.0SD    w1: Mean w1: M-1.0SD 
#>   0.7498826   0.5233201   0.2967576 

# Conditional indirect effects from x1 through m1 and m2 to y,
out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                      wlevels = c("w1", "w4"), fit = fit)
out2
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> m2 -> y
#>  Conditional on moderator(s): w1, w4
#>  Moderator(s) represented by: w1, w4
#> 
#>      [w1]    [w4]   (w1)   (w4)   ind  m1~x m2~m1  y~m2
#> 1 M+1.0SD M+1.0SD  1.228  1.209 0.137 0.750 0.399 0.458
#> 2 M+1.0SD M-1.0SD  1.228 -0.902 0.121 0.750 0.399 0.404
#> 3 M-1.0SD M+1.0SD -0.710  1.209 0.054 0.297 0.399 0.458
#> 4 M-1.0SD M-1.0SD -0.710 -0.902 0.048 0.297 0.399 0.404
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m1~x’,‘m2~m1’,‘y~m2’ is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
#> 
coef(out2)
#> w1: M+1.0SD; w4: M+1.0SD w1: M+1.0SD; w4: M-1.0SD w1: M-1.0SD; w4: M+1.0SD 
#>               0.13702472               0.12079776               0.05422599 
#> w1: M-1.0SD; w4: M-1.0SD 
#>               0.04780435 

# Standardized conditional indirect effects from x1 through m1 and m2 to y,
out2std <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                      wlevels = c("w1", "w4"), fit = fit,
                      standardized_x = TRUE, standardized_y = TRUE)
out2std
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> m2 -> y
#>  Conditional on moderator(s): w1, w4
#>  Moderator(s) represented by: w1, w4
#> 
#>      [w1]    [w4]   (w1)   (w4)   std  m1~x m2~m1  y~m2   ind
#> 1 M+1.0SD M+1.0SD  1.228  1.209 0.031 0.750 0.399 0.458 0.137
#> 2 M+1.0SD M-1.0SD  1.228 -0.902 0.028 0.750 0.399 0.404 0.121
#> 3 M-1.0SD M+1.0SD -0.710  1.209 0.012 0.297 0.399 0.458 0.054
#> 4 M-1.0SD M-1.0SD -0.710 -0.902 0.011 0.297 0.399 0.404 0.048
#> 
#>  - std: The standardized conditional indirect effects. 
#>  - ind: The unstandardized conditional indirect effects.
#>  - ‘m1~x’,‘m2~m1’,‘y~m2’ is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
#> 
coef(out2std)
#> w1: M+1.0SD; w4: M+1.0SD w1: M+1.0SD; w4: M-1.0SD w1: M-1.0SD; w4: M+1.0SD 
#>               0.03124944               0.02754877               0.01236662 
#> w1: M-1.0SD; w4: M-1.0SD 
#>               0.01090212 
```
