# Bootstrap Estimates for 'indirect_effects' and 'cond_indirect_effects'

Generate bootstrap estimates to be used by
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),

## Usage

``` r
do_boot(
  fit,
  R = 100,
  seed = NULL,
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  compute_implied_stats = TRUE
)
```

## Arguments

- fit:

  It can be (a) a list of `lm` class objects, or the output of
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md)
  (i.e., an `lm_list`-class object), or (b) the output of
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html). If it is a
  single model fitted by [`lm()`](https://rdrr.io/r/stats/lm.html), it
  will be automatically converted to a list by
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

- R:

  The number of bootstrap samples. Default is 100.

- seed:

  The seed for the bootstrapping. Default is `NULL` and seed is not set.

- parallel:

  Logical. Whether parallel processing will be used. Default is `TRUE`.

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
  for details. Default is [`list()`](https://rdrr.io/r/base/list.html),
  no additional arguments.

- progress:

  Logical. Display progress or not. Default is `TRUE`.

- compute_implied_stats:

  If `TRUE`, default, implied statistics will be computed for each
  bootstrap sample. Letting users to disable this is an experimental
  features to let the process run faster.

## Value

A `boot_out`-class object that can be used for the `boot_out` argument
of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for forming bootstrap confidence intervals. The object is a list with
the number of elements equal to the number of bootstrap samples. Each
element is a list of the parameter estimates and sample variances and
covariances of the variables in each bootstrap sample.

## Details

It does nonparametric bootstrapping to generate bootstrap estimates of
the parameter estimates in a model fitted either by
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) or by a
sequence of calls to [`lm()`](https://rdrr.io/r/stats/lm.html). The
stored estimates can then be used by
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
to form bootstrapping confidence intervals.

This approach removes the need to repeat bootstrapping in each call to
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
It also ensures that the same set of bootstrap samples is used in all
subsequent analysis.

It determines the type of the fit object automatically and then calls
[`lm2boot_out()`](https://sfcheung.github.io/manymome/reference/lm2boot_out.md),
[`fit2boot_out()`](https://sfcheung.github.io/manymome/reference/fit2boot_out.md),
or
[`fit2boot_out_do_boot()`](https://sfcheung.github.io/manymome/reference/fit2boot_out.md).

### Multigroup Models

Since Version 0.1.14.2, support for multigroup models has been added for
models fitted by `lavaan`. The implementation of bootstrapping is
identical to that used by `lavaan`, with resampling done within each
group.

## See also

[`lm2boot_out()`](https://sfcheung.github.io/manymome/reference/lm2boot_out.md),
[`fit2boot_out()`](https://sfcheung.github.io/manymome/reference/fit2boot_out.md),
and
[`fit2boot_out_do_boot()`](https://sfcheung.github.io/manymome/reference/fit2boot_out.md),
which implements the bootstrapping.

## Examples

``` r
data(data_med_mod_ab1)
dat <- data_med_mod_ab1
lm_m <- lm(m ~ x*w + c1 + c2, dat)
lm_y <- lm(y ~ m*w + x + c1 + c2, dat)
lm_out <- lm2list(lm_m, lm_y)
# In real research, R should be 2000 or even 5000
# In real research, no need to set parallel and progress to FALSE
# Parallel processing is enabled by default and
# progress is displayed by default.
lm_boot_out <- do_boot(lm_out, R = 50, seed = 1234,
                       parallel = FALSE,
                       progress = FALSE)
wlevels <- mod_levels(w = "w", fit = lm_out)
wlevels
#>                w
#> M+1.0SD 6.046455
#> Mean    4.990179
#> M-1.0SD 3.933902
out <- cond_indirect_effects(wlevels = wlevels,
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
#> 1 M+1.0SD 6.046  0.248  0.029 0.452 Sig  0.342 0.725
#> 2 Mean    4.990  0.024 -0.084 0.219      0.063 0.375
#> 3 M-1.0SD 3.934 -0.006 -0.066 0.108     -0.216 0.026
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 50 samples.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
```
