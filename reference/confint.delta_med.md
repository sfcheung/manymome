# Confidence Interval for Delta_Med in a 'delta_med'-Class Object

Return the confidence interval of the Delta_Med in the output of
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md).

## Usage

``` r
# S3 method for class 'delta_med'
confint(object, parm, level = NULL, boot_type, ...)
```

## Arguments

- object:

  The output of
  [`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md).

- parm:

  Not used because only one parameter, the Delta_Med, is allowed.

- level:

  The level of confidence, default is `NULL` and the level used when the
  object was created will be used.

- boot_type:

  If bootstrap confidence interval is to be formed, the type of
  bootstrap confidence interval. The supported types are `"perc"`
  (percentile bootstrap confidence interval, the recommended method) and
  `"bc"` (bias-corrected, or BC, bootstrap confidence interval). If not
  supplied, the stored `boot_type` will be used.

- ...:

  Optional arguments. Ignored.

## Value

A one-row matrix of the confidence interval. All values are `NA` if
bootstrap confidence interval was not requested when calling
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md).

## Details

It returns the nonparametric bootstrap percentile confidence interval of
Delta_Med, proposed byLiu, Yuan, and Li (2023). The object must be the
output of
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md),
with bootstrap confidence interval requested when calling
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md).
However, the level of confidence can be different from that used when
call
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md).

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
```
