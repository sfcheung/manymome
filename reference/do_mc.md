# Monte Carlo Estimates for 'indirect_effects' and 'cond_indirect_effects'

Generate Monte Carlo estimates to be used by
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),

## Usage

``` r
do_mc(
  fit,
  R = 100,
  seed = NULL,
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  compute_implied_stats = TRUE
)

gen_mc_est(fit, R = 100, seed = NULL)
```

## Arguments

- fit:

  The output of
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html). It can
  also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).
  The output of [`stats::lm()`](https://rdrr.io/r/stats/lm.html) is not
  supported.

- R:

  The number of replications. Default is 100.

- seed:

  The seed for the generating Monte Carlo estimates. Default is `NULL`
  and seed is not set.

- parallel:

  Not used. Kept for compatibility with
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).

- ncores:

  Not used. Kept for compatibility with
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).

- make_cluster_args:

  Not used. Kept for compatibility with
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).

- progress:

  Logical. Display progress or not. Default is `TRUE`.

- compute_implied_stats:

  If `TRUE`, default, implied statistics will be computed for each
  replication. Letting users to disable this is an experimental features
  to let the process run faster.

## Value

A `mc_out`-class object that can be used for the `mc_out` argument of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for forming Monte Carlo confidence intervals. The object is a list with
the number of elements equal to the number of Monte Carlo replications.
Each element is a list of the parameter estimates and sample variances
and covariances of the variables in each Monte Carlo replication.

## Details

It uses the parameter estimates and their variance-covariance matrix to
generate Monte Carlo estimates of the parameter estimates in a model
fitted by [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).
The stored estimates can then be used by
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
to form Monte Carlo confidence intervals.

It also supports a model estimated by multiple imputation using
[`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
or its wrapper, such as
[`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).
The pooled estimates and their variance-covariance matrix will be used
to generate the Monte Carlo estimates.

This approach removes the need to repeat Monte Carlo simulation in each
call to
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
It also ensures that the same set of Monte Carlo estimates is used in
all subsequent analysis.

### Multigroup Models

Since Version 0.1.14.2, support for multigroup models has been added for
models fitted by `lavaan`.

## Functions

- `do_mc()`: A general purpose function for creating Monte Carlo
  estimates to be reused by other functions. It returns a `mc_out`-class
  object.

- `gen_mc_est()`: Generate Monte Carlo estimates and store them in the
  `external` slot: `external$manymome$mc`. For advanced users.

## See also

[`fit2mc_out()`](https://sfcheung.github.io/manymome/reference/fit2mc_out.md),
which implements the Monte Carlo simulation.

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
wlevels <- mod_levels(w = "w", fit = fit)
wlevels
#>                w
#> M+1.0SD 6.046455
#> Mean    4.990179
#> M-1.0SD 3.933902
out <- cond_indirect_effects(wlevels = wlevels,
                             x = "x",
                             y = "y",
                             m = "m",
                             fit = fit,
                             mc_ci = TRUE,
                             mc_out = mc_out)
out
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w]   (w)    ind  CI.lo CI.hi Sig    m~x   y~m
#> 1 M+1.0SD 6.046  0.248  0.073 0.469 Sig  0.342 0.725
#> 2 Mean    4.990  0.024 -0.075 0.145      0.063 0.375
#> 3 M-1.0SD 3.934 -0.006 -0.091 0.041     -0.216 0.026
#> 
#>  - [CI.lo to CI.hi] are 95.0% Monte Carlo confidence intervals with 100
#>    replications.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
```
