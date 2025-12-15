# Total Indirect Effect Between Two Variables

Compute the total indirect effect between two variables in the paths
estimated by
[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
total_indirect_effect(object, x, y)
```

## Arguments

- object:

  The output of
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  or a list of `indirect`-class objects.

- x:

  Character. The name of the `x` variable. All paths starting from `x`
  will be included. Can be omitted if all paths have the same `x`.

- y:

  Character. The name of the `y` variable. All paths ending at `y` will
  be included. Can be omitted if all paths have the same `y`.

## Value

An `indirect`-class object.

## Details

It extracts the `indirect`-class objects of relevant paths and then add
the indirect effects together using the `+` operator.

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

# All indirect paths, control variables excluded
paths <- all_indirect_paths(fit,
                            exclude = c("c1", "c2"))
paths
#> Call: 
#> all_indirect_paths(fit = fit, exclude = c("c1", "c2"))
#> Path(s): 
#>   path                
#> 1 m11 -> m12 -> y     
#> 2 x -> m11 -> m12     
#> 3 x -> m11 -> m12 -> y
#> 4 x -> m11 -> y       
#> 5 x -> m12 -> y       
#> 6 x -> m2 -> y        

# Indirect effect estimates
out <- many_indirect_effects(paths,
                             fit = fit)
out
#> 
#> == Indirect Effect(s) ==
#> 
#>                        ind
#> m11 -> m12 -> y      0.241
#> x -> m11 -> m12      0.372
#> x -> m11 -> m12 -> y 0.193
#> x -> m11 -> y        0.163
#> x -> m12 -> y        0.059
#> x -> m2 -> y         0.364
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  

# Total indirect effect from x to y
total_indirect_effect(out,
                      x = "x",
                      y = "y")
#> 
#> == Indirect Effect  ==
#>                                           
#>  Path:                x -> m11 -> m12 -> y
#>  Path:                x -> m11 -> y       
#>  Path:                x -> m12 -> y       
#>  Path:                x -> m2 -> y        
#>  Function of Effects: 0.780               
#> 
#> Computation of the Function of Effects:
#>  (((x->m11->m12->y)
#> +(x->m11->y))
#> +(x->m12->y))
#> +(x->m2->y) 
#> 

```
