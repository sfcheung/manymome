# Conditional, Indirect, and Conditional Indirect Effects

Compute the conditional effects, indirect effects, or conditional
indirect effects in a structural model fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html), or
[`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

## Usage

``` r
cond_indirect(
  x,
  y,
  m = NULL,
  fit = NULL,
  est = NULL,
  implied_stats = NULL,
  wvalues = NULL,
  standardized_x = FALSE,
  standardized_y = FALSE,
  boot_ci = FALSE,
  level = 0.95,
  boot_out = NULL,
  R = 100,
  seed = NULL,
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  save_boot_full = FALSE,
  prods = NULL,
  get_prods_only = FALSE,
  save_boot_out = TRUE,
  mc_ci = FALSE,
  mc_out = NULL,
  save_mc_full = FALSE,
  save_mc_out = TRUE,
  ci_out = NULL,
  save_ci_full = FALSE,
  save_ci_out = TRUE,
  ci_type = NULL,
  group = NULL,
  boot_type = c("perc", "bc"),
  skip_indicators = TRUE,
  internal_options = list()
)

cond_indirect_effects(
  wlevels,
  x,
  y,
  m = NULL,
  fit = NULL,
  w_type = "auto",
  w_method = "sd",
  sd_from_mean = NULL,
  percentiles = NULL,
  est = NULL,
  implied_stats = NULL,
  boot_ci = FALSE,
  R = 100,
  seed = NULL,
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  boot_out = NULL,
  output_type = "data.frame",
  mod_levels_list_args = list(),
  mc_ci = FALSE,
  mc_out = NULL,
  ci_out = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  groups = NULL,
  ...
)

indirect_effect(
  x,
  y,
  m = NULL,
  fit = NULL,
  est = NULL,
  implied_stats = NULL,
  standardized_x = FALSE,
  standardized_y = FALSE,
  boot_ci = FALSE,
  level = 0.95,
  boot_out = NULL,
  R = 100,
  seed = NULL,
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  save_boot_full = FALSE,
  save_boot_out = TRUE,
  mc_ci = FALSE,
  mc_out = NULL,
  save_mc_full = FALSE,
  save_mc_out = TRUE,
  ci_out = NULL,
  save_ci_full = FALSE,
  save_ci_out = TRUE,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  group = NULL,
  skip_indicators = TRUE,
  internal_options = list()
)

cond_effects(
  wlevels,
  x,
  y,
  m = NULL,
  fit = NULL,
  w_type = "auto",
  w_method = "sd",
  sd_from_mean = NULL,
  percentiles = NULL,
  est = NULL,
  implied_stats = NULL,
  boot_ci = FALSE,
  R = 100,
  seed = NULL,
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  make_cluster_args = list(),
  progress = TRUE,
  boot_out = NULL,
  output_type = "data.frame",
  mod_levels_list_args = list(),
  mc_ci = FALSE,
  mc_out = NULL,
  ci_out = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  groups = NULL,
  ...
)

many_indirect_effects(paths, ...)
```

## Arguments

- x:

  Character. The name of the predictor at the start of the path.

- y:

  Character. The name of the outcome variable at the end of the path. If
  the model has only one outcome variable (e.g., moderation only and no
  mediator), then this argument can be omitted.

- m:

  A vector of the variable names of the mediator(s). The path goes from
  the first mediator successively to the last mediator. If `NULL`, the
  default, the path goes from `x` to `y`.

- fit:

  The fit object. Can be a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object or a list of [`lm()`](https://rdrr.io/r/stats/lm.html) outputs.
  It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).
  If it is a single model fitted by
  [`lm()`](https://rdrr.io/r/stats/lm.html), it will be automatically
  converted to a list by
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

- est:

  The output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  If `NULL`, the default, it will be generated from `fit`. If supplied,
  `fit` will be ignored.

- implied_stats:

  Implied means, variances, and covariances of observed variables, of
  the form of the output of
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  with `what` set to `"implied"`. The standard deviations are extracted
  from this object for standardization. Default is `NULL`, and implied
  statistics will be computed from `fit` if required.

- wvalues:

  A numeric vector of named elements. The names are the variable names
  of the moderators, and the values are the values to which the
  moderators will be set to. Default is `NULL`.

- standardized_x:

  Logical. Whether `x` will be standardized. Default is `FALSE`. For
  multigroup models, model implied standard deviation for the selected
  group will be used.

- standardized_y:

  Logical. Whether `y` will be standardized. Default is `FALSE`. For
  multigroup models, model implied standard deviation for the selected
  group will be used.

- boot_ci:

  Logical. Whether bootstrap confidence interval will be formed. Default
  is `FALSE`.

- level:

  The level of confidence for the bootstrap confidence interval. Default
  is .95.

- boot_out:

  If `boot_ci` is `TRUE`, users can supply pregenerated bootstrap
  estimates. This can be the output of
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).
  For `indirect_effect()` and `cond_indirect_effects()`, this can be the
  output of a previous call to `cond_indirect_effects()`,
  `indirect_effect()`, or `cond_indirect()` with bootstrap confidence
  intervals requested. These stored estimates will be reused such that
  there is no need to do bootstrapping again. If not supplied, the
  function will try to generate them from `fit`.

- R:

  Integer. If `boot_ci` is `TRUE`, `boot_out` is `NULL`, and bootstrap
  standard errors not requested if `fit` is a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object, this function will do bootstrapping on `fit`. `R` is the
  number of bootstrap samples. Default is 100. For Monte Carlo
  simulation, this is the number of replications.

- seed:

  If bootstrapping or Monte Carlo simulation is conducted, this is the
  seed for the bootstrapping or simulation. Default is `NULL` and seed
  is not set.

- parallel:

  Logical. If bootstrapping is conducted, whether parallel processing
  will be used. Default is `TRUE`. If `fit` is a list of
  [`lm()`](https://rdrr.io/r/stats/lm.html) outputs, parallel processing
  will not be used.

- ncores:

  Integer. The number of CPU cores to use when `parallel` is `TRUE`.
  Default is the number of non-logical cores minus one (one minimum).
  Will raise an error if greater than the number of cores detected by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  If `ncores` is set, it will override `make_cluster_args` in
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).

- make_cluster_args:

  A named list of additional arguments to be passed to
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html).
  For advanced users. See
  [`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  for details. Default is [`list()`](https://rdrr.io/r/base/list.html).

- progress:

  Logical. Display bootstrapping progress or not. Default is `TRUE`.

- save_boot_full:

  If `TRUE`, full bootstrapping results will be stored. Default is
  `FALSE.`

- prods:

  The product terms found. For internal use.

- get_prods_only:

  IF `TRUE`, will quit early and return the product terms found. The
  results can be passed to the `prod` argument when calling this
  function. Default is `FALSE`. This function is for internal use.

- save_boot_out:

  If `boot_out` is supplied, whether it will be saved in the output.
  Default is `TRUE`.

- mc_ci:

  Logical. Whether Monte Carlo confidence interval will be formed.
  Default is `FALSE`.

- mc_out:

  If `mc_ci` is `TRUE`, users can supply pregenerated Monte Carlo
  estimates. This can be the output of
  [`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md).
  For `indirect_effect()` and `cond_indirect_effects()`, this can be the
  output of a previous call to `cond_indirect_effects()`,
  `indirect_effect()`, or `cond_indirect()` with Monte Carlo confidence
  intervals requested. These stored estimates will be reused such that
  there is no need to do Monte Carlo simulation again. If not supplied,
  the function will try to generate them from `fit`.

- save_mc_full:

  If `TRUE`, full Monte Carlo results will be stored. Default is
  `FALSE.`

- save_mc_out:

  If `mc_out` is supplied, whether it will be saved in the output.
  Default is `TRUE`.

- ci_out:

  If `ci_type` is supplied, this is the corresponding argument. If
  `ci_type` is `"boot"`, this argument will be used as `boot_out`. If
  `ci_type` is `"mc"`, this argument will be used as `mc_out`.

- save_ci_full:

  If `TRUE`, full bootstrapping or Monte Carlo results will be stored.
  Default is `FALSE.`

- save_ci_out:

  If either `mc_out` or `boot_out` is supplied, whether it will be saved
  in the output. Default is `TRUE`.

- ci_type:

  The type of confidence intervals to be formed. Can be either `"boot"`
  (bootstrapping) or `"mc"` (Monte Carlo). If not supplied or is `NULL`,
  will check other arguments (e.g, `boot_ci` and `mc_ci`). If supplied,
  will override `boot_ci` and `mc_ci`.

- group:

  Either the group number as appeared in the
  [`summary()`](https://rdrr.io/r/base/summary.html) or
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  output of a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object, or the group label as used in the
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object. Used only when the number of groups is greater than one.
  Default is `NULL`.

- boot_type:

  If bootstrap confidence interval is to be formed, the type of
  bootstrap confidence interval. The supported types are `"perc"`
  (percentile bootstrap confidence interval, the default and recommended
  type) and `"bc"` (bias-corrected, or BC, bootstrap confidence
  interval).

- skip_indicators:

  Whether observed indicators are skipped from the search for product
  terms. Default is `TRUE`.

- internal_options:

  A named list of internal options. For advanced options.

- wlevels:

  The output of
  [`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md),
  or the moderator(s) to be passed to
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md).
  If all the moderators can be represented by one variable, that is,
  each moderator is (a) a numeric variable, (b) a dichotomous
  categorical variable, or (c) a factor or string variable used in
  [`lm()`](https://rdrr.io/r/stats/lm.html) in `fit`, then it is a
  vector of the names of the moderators as appeared in the data frame.
  If at least one of the moderators is a categorical variable
  represented by more than one variable, such as user-created dummy
  variables used in
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html), then it
  must be a list of the names of the moderators, with such moderators
  represented by a vector of names. For example:
  `list("w1", c("gpgp2", "gpgp3")`, the first moderator `w1` and the
  second moderator a three-categorical variable represented by `gpgp2`
  and `gpgp3`.

- w_type:

  Character. Whether the moderator is a `"numeric"` variable or a
  `"categorical"` variable. If `"auto"`, the function will try to
  determine the type automatically. See
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  for further information.

- w_method:

  Character, either `"sd"` or `"percentile"`. If `"sd"`, the levels are
  defined by the distance from the mean in terms of standard deviation.
  if `"percentile"`, the levels are defined in percentiles. See
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  for further information.

- sd_from_mean:

  A numeric vector. Specify the distance in standard deviation from the
  mean for each level. Default is `c(-1, 0, 1)` when there is only one
  moderator, and `c(-1, 1)` when there are more than one moderator.
  Ignored if `w_method` is not equal to `"sd"`. See
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  for further information.

- percentiles:

  A numeric vector. Specify the percentile (in proportion) for each
  level. Default is `c(.16, .50, .84)` if there is one moderator, and
  `c(.16, .84)` when there are more than one moderator. Ignored if
  `w_method` is not equal to `"percentile"`. See
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  for further information.

- output_type:

  The type of output of `cond_indirect_effects()`. If `"data.frame"`,
  the default, the output will be converted to a data frame. If any
  other values, the output is a list of the outputs from
  `cond_indirect()`.

- mod_levels_list_args:

  Additional arguments to be passed to
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  if it is called for creating the levels of moderators. Default is
  [`list()`](https://rdrr.io/r/base/list.html).

- groups:

  Either a vector of group numbers as appeared in the
  [`summary()`](https://rdrr.io/r/base/summary.html) or
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  output of a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object, or a vector of group labels as used in the
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object. Used only when the number of groups is greater than one.
  Default is `NULL`.

- ...:

  For `many_indirect_effects()`, these are arguments to be passed to
  `indirect_effect()`.

- paths:

  The output of
  [`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)

## Value

`indirect_effect()` and `cond_indirect()` return an `indirect`-class
object.

`cond_indirect_effects()` returns a `cond_indirect_effects`-class
object.

These two classes of objects have their own print methods for printing
the results (see
[`print.indirect()`](https://sfcheung.github.io/manymome/reference/print.indirect.md)
and
[`print.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_effects.md)).
They also have a `coef` method for extracting the estimates
([`coef.indirect()`](https://sfcheung.github.io/manymome/reference/coef.indirect.md)
and
[`coef.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/coef.cond_indirect_effects.md))
and a `confint` method for extracting the confidence intervals
([`confint.indirect()`](https://sfcheung.github.io/manymome/reference/confint.indirect.md)
and
[`confint.cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/confint.cond_indirect_effects.md)).
Addition and subtraction can also be conducted on `indirect`-class
object to estimate and test a function of effects (see
[math_indirect](https://sfcheung.github.io/manymome/reference/math_indirect.md))

## Details

For a model with a mediation path moderated by one or more moderators,
`cond_indirect_effects()` can be used to compute the conditional
indirect effect from one variable to another variable, at one or more
set of selected value(s) of the moderator(s).

If only the effect for one set of value(s) of the moderator(s) is
needed, `cond_indirect()` can be used.

If only the mediator(s) is/are specified (`m`) and no values of
moderator(s) are specified, then the indirect effect from one variable
(`x`) to another variable (`y`) is computed. A convenient wrapper
`indirect_effect()` can be used to compute the indirect effect.

If only the value(s) of moderator(s) is/are specified (`wvalues` or
`wlevels`) and no mediators (`m`) are specified when calling
`cond_indirect_effects()` or `cond_indirect()`, then the conditional
direct effects from one variable to another are computed.

All three functions support using nonparametric bootstrapping (for
`lavaan` or `lm` outputs) or Monte Carlo simulation (for `lavaan`
outputs only) to form confidence intervals. Bootstrapping or Monte Carlo
simulation only needs to be done once. These are the possible ways to
form bootstrapping:

1.  Do bootstrapping or Monte Carlo simulation in the first call to one
    of these functions, by setting `boot_ci` or `mc_ci` to `TRUE` and
    `R` to the number of bootstrap samples or replications, `level` to
    the level of confidence (default .95 or 95%), and `seed` to
    reproduce the results (`parallel` and `ncores` are optional for
    bootstrapping). This will take some time to run for bootstrapping.
    The output will have all bootstrap or Monte Carlo estimates stored.
    This output, whether it is from `indirect_effect()`,
    `cond_indirect_effects()`, or `cond_indirect()`, can be reused by
    any of these three functions by setting `boot_out` (for
    bootstrapping) or `mc_out` (for Monte Carlo simulation) to this
    output. They will form the confidence intervals using the stored
    bootstrap or Monte Carlo estimates.

2.  Do bootstrapping using
    [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
    or Monte Carlo simulation us8ing
    [`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md).
    The output can be used in the `boot_out` (for bootstrapping) or
    `mc_out` (for Monte Carlo simulation) argument of
    `indirect_effect()`, `cond_indirect_effects()` and
    `cond_indirect()`.

3.  For bootstrapping, if
    [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) is used
    to fit a model and `se = "boot"` is used,
    [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
    can extract them to generate a `boot_out`-class object that again
    can be used in the `boot_out` argument.

If `boot_out` or `mc_out` is set, arguments such as `R`, `seed`, and
`parallel` will be ignored.

### Multigroup Models

Since Version 0.1.14.2, support for multigroup models has been added for
models fitted by `lavaan`. Both bootstrapping and Monte Carlo confidence
intervals are supported. When used on a multigroup model:

- For `cond_indirect()` and `indirect_effect()`, users need to specify
  the `group` argument (by number or label). When using
  `cond_indirect_effects()`, if `group` is not set, all groups wil be
  used and the indirect effect in each group will be computed, kind of
  treating group as a moderator.

- For `many_indirect_effects()`, the paths can be generated from a
  multigroup models.

- Currently, `cond_indirect_effects()` does not support a multigroup
  model with moderators on the path selected. The function
  `cond_indirect()` does not have this limitation but users need to
  manually specify the desired value of the moderator(s).

### `many_indirect_effects()`

If bootstrapping or Monte Carlo confidence intervals are requested, it
is advised to use
[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
first to simulate the estimates. Nevertheless, In Version 0.1.14.9 and
later versions, if `boot_ci` or `mc_ci` is `TRUE` when calling
`many_indirect_effects()` but `boot_out` or `mc_out` is not set,
bootstrapping or simulation will be done only once, and then the
bootstrapping or simulated estimates will be used for all paths. This
prevents accidentally repeating the process once for each direct path.

## Functions

- `cond_indirect()`: Compute conditional, indirect, or conditional
  indirect effects for one set of levels.

- `cond_indirect_effects()`: Compute the conditional effects or
  conditional indirect effects for several sets of levels of the
  moderator(s).

- `indirect_effect()`: Compute the indirect effect. A wrapper of
  `cond_indirect()`. Can be used when there is no moderator.

- `cond_effects()`: Just an alias to `cond_indirect_effects()`, a better
  name when a path has no moderator.

- `many_indirect_effects()`: Compute the indirect effects along more
  than one paths. It call `indirect_effect()` once for each of the path.

## See also

[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
and
[`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)
for generating levels of moderators.
[do_boot](https://sfcheung.github.io/manymome/reference/do_boot.md) for
doing bootstrapping before calling these functions.

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x  + d1 * w1 + e1 * x:w1
m2 ~ a2 * x
y  ~ b1 * m1 + b2 * m2 + cp * x
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)
hi_w1 <- mean(dat$w1) + sd(dat$w1)

# Examples for cond_indirect():

# Conditional effect from x to m1 when w1 is 1 SD above mean
cond_indirect(x = "x", y = "m1",
              wvalues = c(w1 = hi_w1), fit = fit)
#> 
#> == Conditional  Effect   ==
#>                                
#>  Path:               x -> m1   
#>  Moderators:         w1        
#>  Conditional Effect: 0.750     
#>  When:               w1 = 1.228
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(1.22806))
#> 

# Direct effect from x to y (direct because no 'm' variables)
indirect_effect(x = "x", y = "y", fit = fit)
#> 
#> ==  Effect  ==
#>                
#>  Path:   x -> y
#>  Effect: 0.312 
#> 
#> Computation Formula:
#>   (b.y~x)
#> 
#> Computation:
#>   (0.31176)
#> 

# Conditional Indirect effect from x1 through m1 to y, when w1 is 1 SD above mean
cond_indirect(x = "x", y = "y", m = "m1",
              wvalues = c(w1 = hi_w1), fit = fit)
#> 
#> == Conditional Indirect Effect   ==
#>                                           
#>  Path:                        x -> m1 -> y
#>  Moderators:                  w1          
#>  Conditional Indirect Effect: -0.031      
#>  When:                        w1 = 1.228  
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.y~m1)
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(1.22806))*(-0.04197)
#> 
#> Coefficients of Component Paths:
#>  Path Conditional Effect Original Coefficient
#>  m1~x              0.750                0.463
#>  y~m1             -0.042               -0.042
#> 



# Examples for cond_indirect_effects():

# Create levels of w1, the moderators
w1levels <- mod_levels("w1", fit = fit)
w1levels
#>                 w1
#> M+1.0SD  1.2280576
#> Mean     0.2589999
#> M-1.0SD -0.7100578

# Conditional effects from x to m1 when w1 is equal to each of the levels
cond_indirect_effects(x = "x", y = "m1",
                      wlevels = w1levels, fit = fit)
#> 
#> == Conditional effects ==
#> 
#>  Path: x -> m1
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>      [w1]   (w1)   ind
#> 1 M+1.0SD  1.228 0.750
#> 2 Mean     0.259 0.523
#> 3 M-1.0SD -0.710 0.297
#> 
#>  - The 'ind' column shows the conditional effects.
#>  

# Conditional Indirect effect from x1 through m1 to y,
# when w1 is equal to each of the levels
cond_indirect_effects(x = "x", y = "y", m = "m1",
                      wlevels = w1levels, fit = fit)
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> y
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>      [w1]   (w1)    ind  m1~x   y~m1
#> 1 M+1.0SD  1.228 -0.031 0.750 -0.042
#> 2 Mean     0.259 -0.022 0.523 -0.042
#> 3 M-1.0SD -0.710 -0.012 0.297 -0.042
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m1~x’,‘y~m1’ is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
#> 

# Multigroup models for cond_indirect_effects()

dat <- data_med_mg
mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE,
           group = "group")

# If a model has more than one group,
# it will be used as a 'moderator'.
cond_indirect_effects(x = "x", y = "y", m = "m",
                      fit = fit)
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on group(s): Group A[1], Group B[2]
#> 
#>     Group Group_ID   ind   m~x   y~m
#> 1 Group A        1 0.409 0.880 0.465
#> 2 Group B        2 0.663 0.597 1.110
#> 
#>  - The 'ind' column shows the indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the group(s).
#> 


# Multigroup model for indirect_effect()

dat <- data_med_mg
mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE,
           group = "group")

# If a model has more than one group,
# the argument 'group' must be set.
ind1 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = fit,
                        group = "Group A")
ind1
#> 
#> == Indirect Effect  ==
#>                                          
#>  Path:            Group A[1]: x -> m -> y
#>  Indirect Effect: 0.409                  
#> 
#> Computation Formula:
#>   (b.m~x)*(b.y~m)
#> 
#> Computation:
#>   (0.87989)*(0.46481)
#> 
#> Coefficients of Component Paths:
#>  Path Coefficient
#>   m~x       0.880
#>   y~m       0.465
#> 
#> NOTE:
#> - The group label is printed before each path.
#> - The group number in square brackets is the number used internally in
#>   lavaan.
#> 
ind2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = fit,
                        group = 2)
ind2
#> 
#> == Indirect Effect  ==
#>                                          
#>  Path:            Group B[2]: x -> m -> y
#>  Indirect Effect: 0.663                  
#> 
#> Computation Formula:
#>   (b.m~x)*(b.y~m)
#> 
#> Computation:
#>   (0.59716)*(1.11040)
#> 
#> Coefficients of Component Paths:
#>  Path Coefficient
#>   m~x       0.597
#>   y~m       1.110
#> 
#> NOTE:
#> - The group label is printed before each path.
#> - The group number in square brackets is the number used internally in
#>   lavaan.
#> 


# Examples for many_indirect_effects():

library(lavaan)
data(data_serial_parallel)
mod <-
"
m11 ~ x + c1 + c2
m12 ~ m11 + x + c1 + c2
m2 ~ x + c1 + c2
y ~ m12 + m2 + m11 + x + c1 + c2
"
fit <- sem(mod, data_serial_parallel,
           fixed.x = FALSE)
# All indirect paths from x to y
paths <- all_indirect_paths(fit,
                           x = "x",
                           y = "y")
paths
#> Call: 
#> all_indirect_paths(fit = fit, x = "x", y = "y")
#> Path(s): 
#>   path                
#> 1 x -> m11 -> m12 -> y
#> 2 x -> m11 -> y       
#> 3 x -> m12 -> y       
#> 4 x -> m2 -> y        
# Indirect effect estimates
out <- many_indirect_effects(paths,
                             fit = fit)
out
#> 
#> == Indirect Effect(s) ==
#> 
#>                        ind
#> x -> m11 -> m12 -> y 0.193
#> x -> m11 -> y        0.163
#> x -> m12 -> y        0.059
#> x -> m2 -> y         0.364
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  

# Multigroup models for many_indirect_effects()

data(data_med_complicated_mg)
mod <-
"
m11 ~ x1 + x2 + c1 + c2
m12 ~ m11 + c1 + c2
m2 ~ x1 + x2 + c1 + c2
y1 ~ m11 + m12 + x1 + x2 + c1 + c2
y2 ~ m2 + x1 + x2 + c1 + c2
"
fit <- sem(mod, data_med_complicated_mg, group = "group")
summary(fit)
#> lavaan 0.6-21 ended normally after 13 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        66
#> 
#>   Number of observations per group:                   
#>     Group A                                        100
#>     Group B                                        100
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                16.359
#>   Degrees of freedom                                14
#>   P-value (Chi-square)                           0.292
#>   Test statistic for each group:
#>     Group A                                      7.443
#>     Group B                                      8.917
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> 
#> Group 1 [Group A]:
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   m11 ~                                               
#>     x1                0.360    0.089    4.037    0.000
#>     x2                0.222    0.103    2.157    0.031
#>     c1                0.275    0.091    3.005    0.003
#>     c2                0.114    0.092    1.240    0.215
#>   m12 ~                                               
#>     m11               0.593    0.088    6.698    0.000
#>     c1                0.030    0.091    0.327    0.743
#>     c2               -0.178    0.089   -1.998    0.046
#>   m2 ~                                                
#>     x1                0.005    0.102    0.045    0.964
#>     x2                0.542    0.117    4.626    0.000
#>     c1                0.082    0.104    0.791    0.429
#>     c2                0.208    0.104    1.992    0.046
#>   y1 ~                                                
#>     m11               0.372    0.119    3.116    0.002
#>     m12               0.351    0.105    3.342    0.001
#>     x1               -0.099    0.098   -1.011    0.312
#>     x2               -0.067    0.107   -0.629    0.529
#>     c1               -0.056    0.097   -0.572    0.567
#>     c2               -0.149    0.096   -1.554    0.120
#>   y2 ~                                                
#>     m2                0.395    0.083    4.771    0.000
#>     x1                0.105    0.084    1.249    0.212
#>     x2                0.178    0.107    1.666    0.096
#>     c1               -0.076    0.087   -0.874    0.382
#>     c2                0.080    0.088    0.912    0.362
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y2               -0.031    0.084   -0.368    0.713
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               0.084    0.096    0.874    0.382
#>    .m12              -0.042    0.093   -0.452    0.651
#>    .m2                0.013    0.109    0.116    0.907
#>    .y1                0.011    0.098    0.108    0.914
#>    .y2               -0.108    0.090   -1.191    0.234
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               0.868    0.123    7.071    0.000
#>    .m12               0.820    0.116    7.071    0.000
#>    .m2                1.126    0.159    7.071    0.000
#>    .y1                0.904    0.128    7.071    0.000
#>    .y2                0.774    0.109    7.071    0.000
#> 
#> 
#> Group 2 [Group B]:
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   m11 ~                                               
#>     x1                0.104    0.107    0.970    0.332
#>     x2               -0.012    0.106   -0.114    0.909
#>     c1                0.364    0.103    3.526    0.000
#>     c2                0.106    0.109    0.978    0.328
#>   m12 ~                                               
#>     m11               0.346    0.096    3.606    0.000
#>     c1                0.219    0.102    2.136    0.033
#>     c2               -0.135    0.105   -1.285    0.199
#>   m2 ~                                                
#>     x1               -0.057    0.104   -0.542    0.588
#>     x2                0.307    0.103    2.965    0.003
#>     c1                0.223    0.101    2.218    0.027
#>     c2                0.181    0.106    1.700    0.089
#>   y1 ~                                                
#>     m11               0.351    0.100    3.509    0.000
#>     m12               0.056    0.098    0.568    0.570
#>     x1                0.080    0.102    0.781    0.435
#>     x2                0.016    0.100    0.157    0.875
#>     c1               -0.294    0.106   -2.782    0.005
#>     c2                0.061    0.104    0.582    0.561
#>   y2 ~                                                
#>     m2                0.398    0.099    4.025    0.000
#>     x1                0.023    0.104    0.224    0.823
#>     x2                0.301    0.107    2.812    0.005
#>     c1                0.110    0.102    1.076    0.282
#>     c2               -0.008    0.107   -0.076    0.940
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y2               -0.077    0.096   -0.805    0.421
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               0.112    0.104    1.075    0.282
#>    .m12               0.149    0.100    1.478    0.139
#>    .m2                0.112    0.101    1.107    0.268
#>    .y1                0.057    0.100    0.575    0.565
#>    .y2                0.198    0.101    1.959    0.050
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .m11               1.044    0.148    7.071    0.000
#>    .m12               0.969    0.137    7.071    0.000
#>    .m2                0.992    0.140    7.071    0.000
#>    .y1                0.933    0.132    7.071    0.000
#>    .y2                0.978    0.138    7.071    0.000
#> 

paths <- all_indirect_paths(fit,
                            x = "x1",
                            y = "y1")
paths
#> Call: 
#> all_indirect_paths(fit = fit, x = "x1", y = "y1")
#> Path(s): 
#>   path                          
#> 1 Group A.x1 -> m11 -> m12 -> y1
#> 2 Group A.x1 -> m11 -> y1       
#> 3 Group B.x1 -> m11 -> m12 -> y1
#> 4 Group B.x1 -> m11 -> y1       
# Indirect effect estimates for all paths in all groups
out <- many_indirect_effects(paths,
                             fit = fit)
out
#> 
#> == Indirect Effect(s) ==
#> 
#>                                  ind
#> Group A.x1 -> m11 -> m12 -> y1 0.075
#> Group A.x1 -> m11 -> y1        0.134
#> Group B.x1 -> m11 -> m12 -> y1 0.002
#> Group B.x1 -> m11 -> y1        0.037
#> 
#>  - The 'ind' column shows the indirect effect(s).
#>  
```
