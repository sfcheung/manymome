# Bootstrap Estimates for a `lavaan` Output

Generate bootstrap estimates from the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

## Usage

``` r
fit2boot_out(fit, compute_implied_stats = TRUE)

fit2boot_out_do_boot(
  fit,
  R = 100,
  seed = NULL,
  parallel = FALSE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  compute_implied_stats = TRUE,
  compute_rsquare = FALSE,
  internal = list()
)
```

## Arguments

- fit:

  The fit object. This function only supports a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object.

- compute_implied_stats:

  If `TRUE`, default, implied statistics will be computed for each
  bootstrap sample. Letting users to disable this is an experimental
  features to let the process run faster.

- R:

  The number of bootstrap samples. Default is 100.

- seed:

  The seed for the random resampling. Default is `NULL`.

- parallel:

  Logical. Whether parallel processing will be used. Default is `NULL`.

- ncores:

  Integer. The number of CPU cores to use when `parallel` is `TRUE`.
  Default is the number of non-logical cores minus one (one minimum).
  Will raise an error if greater than the number of cores detected by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  If `ncores` is set, it will override `make_cluster_args`.

- make_cluster_args:

  A named list of additional arguments to be passed to
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  For advanced users. See
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  for details. Default is [`list()`](https://rdrr.io/r/base/list.html).

- progress:

  Logical. Display progress or not. Default is `TRUE`.

- compute_rsquare:

  If `TRUE`, R-squares will be computed for each bootstrap sample (given
  by
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)).
  Default is `FALSE` because it is rarely necessary, and enabling it
  slows down the computation.

- internal:

  A list of arguments to be used internally for debugging. Default is
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A `boot_out`-class object that can be used for the `boot_out` argument
of
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions for forming bootstrapping confidence intervals.

The object is a list with the number of elements equal to the number of
bootstrap samples. Each element is a list of the parameter estimates and
sample variances and covariances of the variables in each bootstrap
sample.

## Details

This function is for advanced users.
[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
is a function users should try first because
[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
has a general interface for input-specific functions like this one.

If bootstrapping confidence intervals was requested when calling
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) by setting
`se = "boot"`, `fit2boot_out()` can be used to extract the stored
bootstrap estimates so that they can be reused by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and related functions to form bootstrapping confidence intervals for
effects such as indirect effects and conditional indirect effects.

If bootstrapping confidence was not requested when fitting the model by
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html),
`fit2boot_out_do_boot()` can be used to generate nonparametric bootstrap
estimates from the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) and store
them for use by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions.

This approach removes the need to repeat bootstrapping in each call to
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions. It also ensures that the same set of bootstrap
samples is used in all subsequent analyses.

## Functions

- `fit2boot_out()`: Process stored bootstrap estimates for functions
  such as
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- `fit2boot_out_do_boot()`: Do bootstrapping and store information to be
  used by
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  and related functions. Support parallel processing.

## See also

[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md),
the general purpose function that users should try first before using
this function.

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

# Bootstrapping not requested in calling lavaan::sem()
fit <- sem(model = mod, data = dat, fixed.x = FALSE,
           se = "none", baseline = FALSE)
fit_boot_out <- fit2boot_out_do_boot(fit = fit,
                                     R = 40,
                                     seed = 1234,
                                     progress = FALSE)
out <- cond_indirect_effects(wlevels = "w",
                             x = "x",
                             y = "y",
                             m = "m",
                             fit = fit,
                             boot_ci = TRUE,
                             boot_out = fit_boot_out)
out
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w]   (w)    ind  CI.lo CI.hi Sig    m~x   y~m
#> 1 M+1.0SD 6.046  0.248  0.033 0.459 Sig  0.342 0.725
#> 2 Mean    4.990  0.024 -0.065 0.237      0.063 0.375
#> 3 M-1.0SD 3.934 -0.006 -0.069 0.113     -0.216 0.026
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 40 samples.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
```
