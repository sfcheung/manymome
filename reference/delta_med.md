# Delta_Med by Liu, Yuan, and Li (2023)

It computes the Delta_Med proposed by Liu, Yuan, and Li (2023), an
\\R^2\\-like measure of indirect effect.

## Usage

``` r
delta_med(
  x,
  y,
  m,
  fit,
  paths_to_remove = NULL,
  boot_out = NULL,
  level = 0.95,
  progress = TRUE,
  skip_check_single_x = FALSE,
  skip_check_m_between_x_y = FALSE,
  skip_check_x_to_y = FALSE,
  skip_check_latent_variables = FALSE,
  boot_type = c("perc", "bc")
)
```

## Arguments

- x:

  The name of the `x` variable. Must be supplied as a quoted string.

- y:

  The name of the `y` variable. Must be supplied as a quoted string.

- m:

  A vector of the variable names of the mediator(s). If more than one
  mediators, they do not have to be on the same path from `x` to `y`.
  Cannot be `NULL` for this function.

- fit:

  The fit object. Must be a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object.

- paths_to_remove:

  A character vector of paths users want to manually remove, specified
  in `lavaan` model syntax. For example, `c("m2~x", "m3~m2")` removes
  the path from `x` to `m2` and the path from `m2` to `m3`. The default
  is `NULL`, and the paths to remove will be determined using the method
  by Liu et al. (2023). If supplied, then only paths specified
  explicitly will be removed.

- boot_out:

  The output of
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).
  If supplied, the stored bootstrap estimates will be used to form the
  nonparametric percentile bootstrap confidence interval of Delta_Med.

- level:

  The level of confidence of the bootstrap confidence interval. Default
  is .95.

- progress:

  Logical. Display bootstrapping progress or not. Default is `TRUE`.

- skip_check_single_x:

  Logical Check whether the model has one and only one x-variable.
  Default is `TRUE`.

- skip_check_m_between_x_y:

  Logical. Check whether all `m` variables are along a path from `x` to
  `y`. Default is `TRUE`.

- skip_check_x_to_y:

  Logical. Check whether there is a direct path from `x` to `y`. Default
  is `TRUE`.

- skip_check_latent_variables:

  Logical. Check whether the model has any latent variables. Default is
  `TRUE`.

- boot_type:

  If bootstrap confidence interval is to be formed, the type of
  bootstrap confidence interval. The supported types are `"perc"`
  (percentile bootstrap confidence interval, the default and recommended
  type) and `"bc"` (bias-corrected, or BC, bootstrap confidence
  interval).

## Value

A `delta_med` class object. It is a list-like object with these major
elements:

- `delta_med`: The Delta_Med.

- `x`: The name of the `x`-variable.

- `y`: The name of the `y`-variable.

- `m`: A character vector of the mediator(s) along a path. The path runs
  from the first element to the last element.

This class has a `print` method, a `coef` method, and a `confint`
method. See
[`print.delta_med()`](https://sfcheung.github.io/manymome/reference/print.delta_med.md),
[`coef.delta_med()`](https://sfcheung.github.io/manymome/reference/coef.delta_med.md),
and
[`confint.delta_med()`](https://sfcheung.github.io/manymome/reference/confint.delta_med.md).

## Details

It computes Delta_Med, an \\R^2\\-like effect size measure for the
indirect effect from one variable (the `y`-variable) to another variable
(the `x`-variable) through one or more mediators (`m`, or `m1`, `m2`,
etc. when there are more than one mediator).

The Delta_Med of one or more mediators was computed as the difference
between two \\R^2\\s:

- \\R^2_1\\, the \\R^2\\ when `y` is predicted by `x` and all mediators.

- \\R^2_2\\, the \\R^2\\ when the mediator(s) of interest is/are removed
  from the models, while the error term(s) of the mediator(s) is/are
  kept.

Delta_Med is given by \\R^2_1 - R^2_2\\.

Please refer to Liu et al. (2023) for the technical details.

The function can also form a nonparametric percentile bootstrap
confidence of Delta_Med.

## Implementation

The function identifies all the path(s) pointing to the mediator(s) of
concern and fixes the path(s) to zero, effectively removing the
mediator(s). However, the model is not refitted, hence keeping the
estimates of all other parameters unchanged. It then uses
[`lavaan::lav_model_set_parameters()`](https://rdrr.io/pkg/lavaan/man/lav_model.html)
to update the parameters,
[`lavaan::lav_model_implied()`](https://rdrr.io/pkg/lavaan/man/lav_model.html)
to update the implied statistics, and then calls
[`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
to retrieve the implied variance of the predicted values of `y` for
computing the \\R^2_2\\. Subtracting this \\R^2_2\\ from \\R^2_1\\ of
`y` can then yield Delta_Med.

## Model Requirements

For now, by default, it only computes Delta_Med for the types of models
discussed in Liu et al. (2023):

- Having one predictor (the `x`-variable).

- Having one or more mediators, the `m`-variables, with arbitrary way to
  mediate the effect of `x` on the outcome variable (`y`-variable).

- Having one or more outcome variables. Although their models only have
  outcome variables, the computation of the Delta_Med is not affected by
  the presence of other outcome variables.

- Having no control variables.

- The mediator(s), `m`, and the `y`-variable are continuous.

- `x` can be continuous or categorical. If categorical, it needs to be
  handle appropriately when fitting the model.

- `x` has a direct path to `y`.

- All the mediators listed in the argument `m` is present in at least
  one path from `x` to `y.`

- None of the paths from `x` to `y` are moderated.

It can be used for other kinds of models but support for them is
disabled by default. To use this function for cases not discussed in Liu
et al. (2023), please disable relevant requirements stated above using
the relevant `skip_check_*` arguments. An error will be raised if the
models failed any of the checks not skipped by users.

## References

Liu, H., Yuan, K.-H., & Li, H. (2025). A systematic framework for
defining R-squared measures in mediation analysis. *Psychological
Methods*, *30*(2), 306-321. https://doi.org/10.1037/met0000571

## See also

[`print.delta_med()`](https://sfcheung.github.io/manymome/reference/print.delta_med.md),
[`coef.delta_med()`](https://sfcheung.github.io/manymome/reference/coef.delta_med.md),
and
[`confint.delta_med()`](https://sfcheung.github.io/manymome/reference/confint.delta_med.md).

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

```
