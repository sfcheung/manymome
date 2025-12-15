# Get The Conditional Indirect Effect for One Row of 'cond_indirect_effects' Output

Return the conditional indirect effect of one row of the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
get_one_cond_indirect_effect(object, row)

get_one_cond_effect(object, row)

print_all_cond_indirect_effects(object, ...)

print_all_cond_effects(object, ...)
```

## Arguments

- object:

  The output of
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- row:

  The row number of the row to be retrieved.

- ...:

  Optional arguments to be passed to teh `print` method of the output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  and
  [`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Value

`get_one_cond_indirect_effect()` returns an `indirect`-class object,
similar to the output of
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
See
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for details on these classes.

`print_all_cond_indirect_effects()` returns the object invisibly. Called
for its side effect.

## Details

`get_one_cond_indirect_effect()` extracts the corresponding output of
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
from the requested row.

`get_one_cond_effect()` is an alias of `get_one_cond_indirect_effect()`.

`print_all_cond_indirect_effects()` loops over the conditional effects
and print all of them.

`print_all_cond_effects()` is an alias of
`print_all_cond_indirect_effects()`.

## See also

[cond_indirect_effects](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

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

# Examples for cond_indirect():

# Conditional effects from x to m1
# when w1 is equal to each of the default levels
out1 <- cond_indirect_effects(x = "x", y = "m1",
                              wlevels = c("w1", "w4"), fit = fit)
get_one_cond_indirect_effect(out1, 3)
#> 
#> == Conditional  Effect   ==
#>                                             
#>  Path:               x -> m1                
#>  Moderators:         w1, w4                 
#>  Conditional Effect: 0.297                  
#>  When:               w1 = -0.710, w4 = 1.209
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(-0.71006))
#> 

# Conditional Indirect effect from x1 through m1 to y,
# when w1 is equal to each of the levels
out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                              wlevels = c("w1", "w4"), fit = fit)
get_one_cond_indirect_effect(out2, 4)
#> 
#> == Conditional Indirect Effect   ==
#>                                                       
#>  Path:                        x -> m1 -> m2 -> y      
#>  Moderators:                  w1, w4                  
#>  Conditional Indirect Effect: 0.048                   
#>  When:                        w1 = -0.710, w4 = -0.902
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.m2~m1)*(b.y~m2 + (b.m2:w4)*(w4))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(-0.71006))*(0.39883)*((0.42709) +
#>   (0.02570)*(-0.90228))
#> 
#> Coefficients of Component Paths:
#>   Path Conditional Effect Original Coefficient
#>   m1~x              0.297                0.463
#>  m2~m1              0.399                0.399
#>   y~m2              0.404                0.427
#> 


print_all_cond_indirect_effects(out2, digits = 2)
#> ---------------------------------------------------------------- 
#> Moderator Level: w1: M+1.0SD; w4: M+1.0SD 
#> 
#> == Conditional Indirect Effect   ==
#>                                                   
#>  Path:                        x -> m1 -> m2 -> y  
#>  Moderators:                  w1, w4              
#>  Conditional Indirect Effect: 0.14                
#>  When:                        w1 = 1.23, w4 = 1.21
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.m2~m1)*(b.y~m2 + (b.m2:w4)*(w4))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(1.22806))*(0.39883)*((0.42709) +
#>   (0.02570)*(1.20878))
#> 
#> Coefficients of Component Paths:
#>   Path Conditional Effect Original Coefficient
#>   m1~x               0.75                 0.46
#>  m2~m1               0.40                 0.40
#>   y~m2               0.46                 0.43
#> 
#> ---------------------------------------------------------------- 
#> Moderator Level: w1: M+1.0SD; w4: M-1.0SD 
#> 
#> == Conditional Indirect Effect   ==
#>                                                    
#>  Path:                        x -> m1 -> m2 -> y   
#>  Moderators:                  w1, w4               
#>  Conditional Indirect Effect: 0.12                 
#>  When:                        w1 = 1.23, w4 = -0.90
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.m2~m1)*(b.y~m2 + (b.m2:w4)*(w4))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(1.22806))*(0.39883)*((0.42709) +
#>   (0.02570)*(-0.90228))
#> 
#> Coefficients of Component Paths:
#>   Path Conditional Effect Original Coefficient
#>   m1~x               0.75                 0.46
#>  m2~m1               0.40                 0.40
#>   y~m2               0.40                 0.43
#> 
#> ---------------------------------------------------------------- 
#> Moderator Level: w1: M-1.0SD; w4: M+1.0SD 
#> 
#> == Conditional Indirect Effect   ==
#>                                                    
#>  Path:                        x -> m1 -> m2 -> y   
#>  Moderators:                  w1, w4               
#>  Conditional Indirect Effect: 0.05                 
#>  When:                        w1 = -0.71, w4 = 1.21
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.m2~m1)*(b.y~m2 + (b.m2:w4)*(w4))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(-0.71006))*(0.39883)*((0.42709) +
#>   (0.02570)*(1.20878))
#> 
#> Coefficients of Component Paths:
#>   Path Conditional Effect Original Coefficient
#>   m1~x               0.30                 0.46
#>  m2~m1               0.40                 0.40
#>   y~m2               0.46                 0.43
#> 
#> ---------------------------------------------------------------- 
#> Moderator Level: w1: M-1.0SD; w4: M-1.0SD 
#> 
#> == Conditional Indirect Effect   ==
#>                                                     
#>  Path:                        x -> m1 -> m2 -> y    
#>  Moderators:                  w1, w4                
#>  Conditional Indirect Effect: 0.05                  
#>  When:                        w1 = -0.71, w4 = -0.90
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.m2~m1)*(b.y~m2 + (b.m2:w4)*(w4))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(-0.71006))*(0.39883)*((0.42709) +
#>   (0.02570)*(-0.90228))
#> 
#> Coefficients of Component Paths:
#>   Path Conditional Effect Original Coefficient
#>   m1~x                0.3                 0.46
#>  m2~m1                0.4                 0.40
#>   y~m2                0.4                 0.43
#> 
```
