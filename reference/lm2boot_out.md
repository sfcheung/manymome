# Bootstrap Estimates for `lm` Outputs

Generate bootstrap estimates for models in a list of 'lm' outputs.

## Usage

``` r
lm2boot_out(
  outputs,
  R = 100,
  seed = NULL,
  progress = TRUE,
  compute_implied_stats = TRUE
)

lm2boot_out_parallel(
  outputs,
  R = 100,
  seed = NULL,
  parallel = FALSE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  compute_implied_stats = TRUE
)
```

## Arguments

- outputs:

  A list of `lm` class objects, or the output of
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md)
  (i.e., an `lm_list`-class object).

- R:

  The number of bootstrap samples. Default is 100.

- seed:

  The seed for the random resampling. Default is `NULL`.

- progress:

  Logical. Display progress or not. Default is `TRUE`.

- compute_implied_stats:

  If `TRUE`, default, implied statistics will be computed for each
  bootstrap sample. Letting users to disable this is an experimental
  features to let the process run faster.

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

It does nonparametric bootstrapping to generate bootstrap estimates of
the regression coefficients in the regression models of a list of
[`lm()`](https://rdrr.io/r/stats/lm.html) outputs, or an `lm_list`-class
object created by
[`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).
The stored estimates can be used by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions in forming bootstrapping confidence intervals for
effects such as indirect effect and conditional indirect effects.

This approach removes the need to repeat bootstrapping in each call to
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and related functions. It also ensures that the same set of bootstrap
samples is used in all subsequent analyses.

## Functions

- `lm2boot_out()`: Generate bootstrap estimates using one process
  (serial, without parallelization).

- `lm2boot_out_parallel()`: Generate bootstrap estimates using parallel
  processing.

## See also

[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md),
the general purpose function that users should try first before using
this function.

## Examples

``` r
data(data_med_mod_ab1)
dat <- data_med_mod_ab1
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
lm_out <- lm2list(lm_m, lm_y)
# In real research, R should be 2000 or even 5000
# In real research, no need to set progress to FALSE
# Progress is displayed by default.
lm_boot_out <- lm2boot_out(lm_out, R = 100, seed = 1234,
                           progress = FALSE)
out <- cond_indirect_effects(wlevels = "w",
                             x = "x",
                             y = "y",
                             m = "m",
                             fit = lm_out,
                             boot_ci = TRUE,
                             boot_out = lm_boot_out)
out
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w]   (w)    ind  CI.lo CI.hi Sig    m~x   y~m
#> 1 M+1.0SD 6.046  0.248  0.030 0.458 Sig  0.342 0.725
#> 2 Mean    4.990  0.024 -0.079 0.171      0.063 0.375
#> 3 M-1.0SD 3.934 -0.006 -0.087 0.098     -0.216 0.026
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 100 samples.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
```
