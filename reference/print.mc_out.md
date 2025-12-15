# Print a `mc_out`-Class Object

Print the content of the output of
[`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md) or
related functions.

## Usage

``` r
# S3 method for class 'mc_out'
print(x, ...)
```

## Arguments

- x:

  The output of
  [`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md),
  or any `mc_out`-class object returned by similar functions.

- ...:

  Other arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Examples

``` r
library(lavaan)
data(data_med_mod_ab1)
dat <- data_med_mod_ab1
mod <-
"
m ~ x + w + x:w + c1 + c2
y ~ m + w + m:w + x + c1 + c2
"
fit <- sem(mod, dat)
# In real research, R should be 5000 or even 10000
mc_out <- do_mc(fit, R = 100, seed = 1234)
#> Stage 1: Simulate estimates
#> Stage 2: Compute implied statistics

# Print the output of do_boot()
mc_out
#> 
#> == A 'mc_out' class object ==
#> 
#> Number of Monte Carlo replications: 100 
#> Column names in data:
#> [1] m   y   m:w x   w   x:w c1  c2 
#> (Note: May contain derived terms such as product terms.)
#> 
```
