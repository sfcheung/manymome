# Print a 'delta_med' Class Object

Print the content of a `delta_med`-class object.

## Usage

``` r
# S3 method for class 'delta_med'
print(x, digits = 3, level = NULL, full = FALSE, boot_type, ...)
```

## Arguments

- x:

  A `delta_med`-class object.

- digits:

  The number of digits after the decimal. Default is 3.

- level:

  The level of confidence of bootstrap confidence interval, if requested
  when created. If `NULL`, the default, the level requested when calling
  [`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md)
  is used. If not null, then this level will be used.

- full:

  Logical. Whether additional information will be printed. Default is
  `FALSE`.

- boot_type:

  If bootstrap confidence interval is to be formed, the type of
  bootstrap confidence interval. The supported types are `"perc"`
  (percentile bootstrap confidence interval, the recommended method) and
  `"bc"` (bias-corrected, or BC, bootstrap confidence interval). If not
  supplied, the stored `boot_type` will be used.

- ...:

  Optional arguments. Ignored.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

It prints the output of
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md),
which is a `delta_med`-class object.

## See also

[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
dat <- data_med
mod <-
"
m ~ x
y ~ m + x
"
fit <- sem(mod, dat)
dm <- delta_med(x = "x",
                y = "y",
                m = "m",
                fit = fit)
dm
#> Call:
#> delta_med(x = "x", y = "y", m = "m", fit = fit)
#> 
#> Predictor (x)       : x 
#> Mediator(s) (m)     : m 
#> Outcome variable (y): y 
#> 
#> Delta_med: 0.230
#> 
#> Paths removed:
#>  m~x
print(dm, full = TRUE)
#> Call:
#> delta_med(x = "x", y = "y", m = "m", fit = fit)
#> 
#> Predictor (x)       : x 
#> Mediator(s) (m)     : m 
#> Outcome variable (y): y 
#> 
#> Delta_med: 0.230
#> 
#> Paths removed:
#>  m~x
#> 
#> Additional information:
#> R-sq: Original                            : 0.351
#> R-sq: Mediator(s) removed                 : 0.121
#> Variance of y                             : 6.273
#> Variance of predicted y                   : 2.203
#> Variance of predicted: mediator(s) removed: 0.759

# Call do_boot() to generate
# bootstrap estimates
# Use 2000 or even 5000 for R in real studies
# Set parallel to TRUE in real studies for faster bootstrapping
boot_out <- do_boot(fit,
                    R = 45,
                    seed = 879,
                    parallel = FALSE,
                    progress = FALSE)
# Remove 'progress = FALSE' in practice
dm_boot <- delta_med(x = "x",
                     y = "y",
                     m = "m",
                     fit = fit,
                     boot_out = boot_out,
                     progress = FALSE)
dm_boot
#> Call:
#> delta_med(x = "x", y = "y", m = "m", fit = fit, boot_out = boot_out, 
#>     progress = FALSE)
#> 
#> Predictor (x)       : x 
#> Mediator(s) (m)     : m 
#> Outcome variable (y): y 
#> 
#> Delta_med                                     :          0.230
#> 95.0% Bootstrap percentile confidence interval: [0.097, 0.318]
#> Number of bootstrap samples                   :             45
#> 
#> Paths removed:
#>  m~x
confint(dm_boot)
#>           Percentile 2.5 % Percentile 97.5 %
#> Delta_Med       0.09725932         0.3175632
confint(dm_boot,
        level = .90)
#>           Percentile 5 % Percentile 95 %
#> Delta_Med       0.121015        0.294301
```
