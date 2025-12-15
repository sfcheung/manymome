# Extract the Indirect Effects from a 'indirect_list' Object

Return the estimates of the indirect effects in the output of
[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'indirect_list'
coef(object, ...)
```

## Arguments

- object:

  The output of
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- ...:

  Optional arguments. Ignored by the function.

## Value

A numeric vector of the indirect effects.

## Details

It extracts the estimates in each 'indirect'-class object in the list.

If standardized effect is requested when calling
[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
the effects returned are also standardized.

## See also

[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Examples

``` r
library(lavaan)
data(data_serial_parallel)
mod <-
"
m11 ~ x + c1 + c2
m12 ~ m11 + x + c1 + c2
m2 ~ x + c1 + c2
y ~ m12 + m2 + m11 + x + c1 + c2
"
fit <- sem(mod, data_serial_parallel,
           fixed.x = FALSE)
# All indirect paths from x to y
paths <- all_indirect_paths(fit,
                           x = "x",
                           y = "y")
paths
#> Call: 
#> all_indirect_paths(fit = fit, x = "x", y = "y")
#> Path(s): 
#>   path                
#> 1 x -> m11 -> m12 -> y
#> 2 x -> m11 -> y       
#> 3 x -> m12 -> y       
#> 4 x -> m2 -> y        
# Indirect effect estimates
out <- many_indirect_effects(paths,
                             fit = fit)
out
#> 
#> == Indirect Effect(s) ==
#> 
#>                        ind
#> x -> m11 -> m12 -> y 0.193
#> x -> m11 -> y        0.163
#> x -> m12 -> y        0.059
#> x -> m2 -> y         0.364
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
coef(out)
#> x -> m11 -> m12 -> y        x -> m11 -> y        x -> m12 -> y 
#>           0.19321379           0.16261213           0.05946653 
#>         x -> m2 -> y 
#>           0.36440188 

```
