# Print a `boot_out`-Class Object

Print the content of the output of
[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
or related functions.

## Usage

``` r
# S3 method for class 'boot_out'
print(x, ...)
```

## Arguments

- x:

  The output of
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md),
  or any `boot_out`-class object returned by similar functions.

- ...:

  Other arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Examples

``` r
data(data_med_mod_ab1)
dat <- data_med_mod_ab1
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
lm_out <- lm2list(lm_m, lm_y)
# In real research, R should be 2000 or even 5000
# In real research, no need to set parallel to FALSE
# In real research, no need to set progress to FALSE
# Progress is displayed by default.
lm_boot_out <- do_boot(lm_out, R = 100,
                       seed = 1234,
                       progress = FALSE,
                       parallel = FALSE)
# Print the output of do_boot()
lm_boot_out
#> 
#> == A 'boot_out' class object ==
#> 
#> Number of bootstrap samples: 100 
#> Number of cases in each bootstrap sample: 100 
#> (Missing data, if any, is ignored in the count.)
#> Column names in data:
#> [1] x   w   c1  c2  x:w m   m:w y  
#> (Note: May contain derived terms such as product terms.)
#> 

library(lavaan)
data(data_med_mod_ab1)
dat <- data_med_mod_ab1
dat$"x:w" <- dat$x * dat$w
dat$"m:w" <- dat$m * dat$w
mod <-
"
m ~ x + w + x:w + c1 + c2
y ~ m + w + m:w + x + c1 + c2
"
fit <- sem(model = mod, data = dat, fixed.x = FALSE,
           se = "none", baseline = FALSE)
# In real research, R should be 2000 or even 5000
# In real research, no need to set progress to FALSE
# In real research, no need to set parallel to FALSE
# Progress is displayed by default.
fit_boot_out <- do_boot(fit = fit,
                        R = 40,
                        seed = 1234,
                        parallel = FALSE,
                        progress = FALSE)
# Print the output of do_boot()
fit_boot_out
#> 
#> == A 'boot_out' class object ==
#> 
#> Number of bootstrap samples: 40 
#> Column names in data:
#> [1] m   y   m:w x   w   x:w c1  c2 
#> (Note: May contain derived terms such as product terms.)
#> 
```
