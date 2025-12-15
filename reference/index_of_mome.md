# Index of Moderated Mediation and Index of Moderated Moderated Mediation

It computes the index of moderated mediation and the index of moderated
moderated mediation proposed by Hayes (2015, 2018).

## Usage

``` r
index_of_mome(
  x,
  y,
  m = NULL,
  w = NULL,
  fit = NULL,
  boot_ci = FALSE,
  level = 0.95,
  boot_out = NULL,
  R = 100,
  seed = NULL,
  progress = TRUE,
  mc_ci = FALSE,
  mc_out = NULL,
  ci_type = NULL,
  ci_out = NULL,
  boot_type = c("perc", "bc"),
  skip_indicators = TRUE,
  ...
)

index_of_momome(
  x,
  y,
  m = NULL,
  w = NULL,
  z = NULL,
  fit = NULL,
  boot_ci = FALSE,
  level = 0.95,
  boot_out = NULL,
  R = 100,
  seed = NULL,
  progress = TRUE,
  mc_ci = FALSE,
  mc_out = NULL,
  ci_type = NULL,
  ci_out = NULL,
  boot_type = c("perc", "bc"),
  skip_indicators = TRUE,
  ...
)
```

## Arguments

- x:

  Character. The name of the predictor at the start of the path.

- y:

  Character. The name of the outcome variable at the end of the path.

- m:

  A vector of the variable names of the mediator(s). The path goes from
  the first mediator successively to the last mediator. If `NULL`, the
  default, the path goes from `x` to `y`.

- w:

  Character. The name of the moderator.

- fit:

  The fit object. Can be a `lavaan::lavaan-class` object, a list of
  [`lm()`](https://rdrr.io/r/stats/lm.html) outputs, or an object
  created by
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).
  It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

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
  For
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  and
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  this can be the output of a previous call to
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  or
  [`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  with bootstrap confidence intervals requested. These stored estimates
  will be reused such that there is no need to do bootstrapping again.
  If not supplied, the function will try to generate them from `fit`.

- R:

  Integer. If `boot_ci` is `TRUE`, `boot_out` is `NULL`, and bootstrap
  standard errors not requested if `fit` is a `lavaan-class` object,
  this function will do bootstrapping on `fit`. `R` is the number of
  bootstrap samples. Default is 100. For Monte Carlo simulation, this is
  the number of replications.

- seed:

  If bootstrapping or Monte Carlo simulation is conducted, this is the
  seed for the bootstrapping or simulation. Default is `NULL` and seed
  is not set.

- progress:

  Logical. Display bootstrapping progress or not. Default is `TRUE`.

- mc_ci:

  Logical. Whether Monte Carlo confidence interval will be formed.
  Default is `FALSE`.

- mc_out:

  If `mc_ci` is `TRUE`, users can supply pregenerated Monte Carlo
  estimates. This can be the output of
  [`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md).
  For
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  and
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  this can be the output of a previous call to
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
  or
  [`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  with Monte Carlo confidence intervals requested. These stored
  estimates will be reused such that there is no need to do Monte Carlo
  simulation again. If not supplied, the function will try to generate
  them from `fit`.

- ci_type:

  The type of confidence intervals to be formed. Can be either `"boot"`
  (bootstrapping) or `"mc"` (Monte Carlo). If not supplied or is `NULL`,
  will check other arguments (e.g, `boot_ci` and `mc_ci`). If supplied,
  will override `boot_ci` and `mc_ci`.

- ci_out:

  If `ci_type` is supplied, this is the corresponding argument. If
  `ci_type` is `"boot"`, this argument will be used as `boot_out`. If
  `ci_type` is `"mc"`, this argument will be used as `mc_out`.

- boot_type:

  If bootstrap confidence interval is to be formed, the type of
  bootstrap confidence interval. The supported types are `"perc"`
  (percentile bootstrap confidence interval, the default and recommended
  type) and `"bc"` (bias-corrected, or BC, bootstrap confidence
  interval).

- skip_indicators:

  Whether observed indicators are skipped from the search for product
  terms. Default is `TRUE`.

- ...:

  Arguments to be passed to
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

- z:

  Character. The name of the second moderator, for computing the index
  of moderated moderated mediation.

## Value

It returns a `cond_indirect_diff`-class object. This class has a `print`
method
([`print.cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_diff.md)),
a `coef` method for extracting the index
([`coef.cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/coef.cond_indirect_diff.md)),
and a `confint` method for extracting the confidence interval if
available
([`confint.cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/confint.cond_indirect_diff.md)).

## Details

The function `index_of_mome()` computes the *index of moderated
mediation* proposed by Hayes (2015). It supports any path in a model
with one (and only one) component path moderated. For example,
`x->m1->m2->y` with `x->m1` moderated by `w`. It measures the change in
indirect effect when the moderator increases by one unit.

The function `index_of_momome()` computes the *index of moderated
moderated mediation* proposed by Hayes (2018). It supports any path in a
model, with two component paths moderated, each by one moderator. For
example, `x->m1->m2->y` with `x->m1` moderated by `w` and `m2->y`
moderated by `z`. It measures the change in the index of moderated
mediation of one moderator when the other moderator increases by one
unit.

## Functions

- `index_of_mome()`: Compute the index of moderated mediation.

- `index_of_momome()`: Compute the index of moderated moderated
  mediation.

## References

Hayes, A. F. (2015). An index and test of linear moderated mediation.
*Multivariate Behavioral Research, 50*(1), 1-22.
[doi:10.1080/00273171.2014.962683](https://doi.org/10.1080/00273171.2014.962683)

Hayes, A. F. (2018). Partial, conditional, and moderated moderated
mediation: Quantification, inference, and interpretation. *Communication
Monographs, 85*(1), 4-40.
[doi:10.1080/03637751.2017.1352100](https://doi.org/10.1080/03637751.2017.1352100)

## See also

[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
mod <-
"
m1 ~ a * x  + f * w1 + d * xw1
y  ~ b * m1 + cp * x
ind_mome := d * b
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# R should be at least 2000 or even 5000 in real research.
# parallel is set to TRUE by default.
# Therefore, in research, the argument parallel can be omitted.
out_mome <- index_of_mome(x = "x", y = "y", m = "m1", w = "w1",
                          fit = fit,
                          boot_ci = TRUE,
                          R = 42,
                          seed = 4314,
                          parallel = FALSE,
                          progress = FALSE)
out_mome
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> y
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>   [w1] (w1)   ind  CI.lo CI.hi Sig  m1~x  y~m1
#> 1    1    1 0.063 -0.693 0.491     0.697 0.091
#> 2    0    0 0.042 -0.439 0.330     0.463 0.091
#> 
#> == Index of Moderated Mediation ==
#> 
#> Levels compared: Row 1 - Row 2
#> 
#>       x y Index  CI.lo CI.hi
#> Index x y 0.021 -0.254 0.171
#> 
#>  - [CI.lo, CI.hi]: 95% percentile confidence interval.
#> 
coef(out_mome)
#>     y~m1~x 
#> 0.02126581 
# From lavaan
print(est[19, ], nd = 8)
#>         lhs op rhs    label        est
#> 19 ind_mome := d*b ind_mome 0.02126581
confint(out_mome)
#>             2.50%    97.50%
#> y~m1~x -0.2540199 0.1710191



library(lavaan)
dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
dat$m1w4 <- dat$m1 * dat$w4
mod <-
"
m1 ~ a * x  + f1 * w1 + d1 * xw1
y  ~ b * m1 + f4 * w4 + d4 * m1w4 + cp * x
ind_momome := d1 * d4
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# See the example of index_of_mome on how to request
# bootstrap confidence interval.
out_momome <- index_of_momome(x = "x", y = "y", m = "m1",
                              w = "w1", z = "w4",
                              fit = fit)
out_momome
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> y
#>  Conditional on moderator(s): w1, w4
#>  Moderator(s) represented by: w1, w4
#> 
#>   [w1] [w4] (w1) (w4)    ind  m1~x   y~m1
#> 1    1    1    1    1 -0.119 0.697 -0.172
#> 2    1    0    1    0 -0.070 0.697 -0.101
#> 3    0    1    0    1 -0.079 0.463 -0.172
#> 4    0    0    0    0 -0.047 0.463 -0.101
#> 
#> == Index of Moderated Moderated Mediation ==
#> 
#> Levels compared:
#> (Row 1 - Row 2) - (Row 3 - Row 4)
#> 
#>       x y  Index
#> Index x y -0.017
#> 
#>  
coef(out_momome)
#>      y~m1~x 
#> -0.01659799 
print(est[32, ], nd = 8)
#>           lhs op   rhs      label         est
#> 32 ind_momome := d1*d4 ind_momome -0.01659799
```
