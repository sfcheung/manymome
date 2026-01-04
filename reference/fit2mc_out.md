# Monte Carlo Estimates for a `lavaan` Output

Generate Monte Carlo estimates from the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

## Usage

``` r
fit2mc_out(fit, progress = TRUE, compute_implied_stats = TRUE)
```

## Arguments

- fit:

  The fit object. This function only supports a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object. It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

- progress:

  Logical. Display progress or not. Default is `TRUE`.

- compute_implied_stats:

  If `TRUE`, default, implied statistics will be computed for each
  replication. Letting users to disable this is an experimental features
  to let the process run faster.

## Value

A `mc_out`-class object that can be used for the `mc_out` argument of
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions for forming Monte Carlo confidence intervals.

The object is a list with the number of elements equal to the number of
Monte Carlo replications. Each element is a list of the parameter
estimates and sample variances and covariances of the variables in each
Monte Carlo replication.

## Details

This function is for advanced users.
[`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md) is a
function users should try first because
[`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md) has
a general interface for input-specific functions like this one.

`fit2mc_out()` can be used to extract the stored Monte Carlo estimates
so that they can be reused by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and related functions to form Monte Carlo confidence intervals for
effects such as indirect effects and conditional indirect effects.

This approach removes the need to repeat Monte Carlo simulation in each
call to
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions. It also ensures that the same set of Monte Carlo
estimates is used in all subsequent analyses.

## See also

[`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md), the
general purpose function that users should try first before using this
function.

## Examples

``` r
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
           baseline = FALSE)
# In real research, R should be 5000 or even 10000.
fit <- gen_mc_est(fit, R = 100, seed = 453253)
fit_mc_out <- fit2mc_out(fit)
#> Stage 1: Simulate estimates
#> Stage 2: Compute implied statistics
out <- cond_indirect_effects(wlevels = "w",
                             x = "x",
                             y = "y",
                             m = "m",
                             fit = fit,
                             mc_ci = TRUE,
                             mc_out = fit_mc_out)
out
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w]   (w)    ind  CI.lo CI.hi Sig    m~x   y~m
#> 1 M+1.0SD 6.046  0.248  0.030 0.470 Sig  0.342 0.725
#> 2 Mean    4.990  0.024 -0.066 0.117      0.063 0.375
#> 3 M-1.0SD 3.934 -0.006 -0.074 0.029     -0.216 0.026
#> 
#>  - [CI.lo to CI.hi] are 95.0% Monte Carlo confidence intervals with 100
#>    replications.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
```
