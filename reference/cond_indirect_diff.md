# Differences In Conditional Indirect Effects

Compute the difference in conditional indirect effects between two sets
of levels of the moderators.

## Usage

``` r
cond_indirect_diff(output, from = NULL, to = NULL, level = 0.95)
```

## Arguments

- output:

  A `cond_indirect_effects`-class object: The output of
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- from:

  A row number of `output`.

- to:

  A row number of `output`. The change in indirect effects is computed
  by the change in the level(s) of the moderator(s) from Row `from` to
  Row `to`.

- level:

  The level of confidence for the confidence interval. Default is .95.

## Value

A `cond_indirect_diff`-class object. This class has a `print` method
([`print.cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_diff.md)),
a `coef` method
([`coef.cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/coef.cond_indirect_diff.md)),
and a `confint` method
([`confint.cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/confint.cond_indirect_diff.md)).

## Details

Ths function takes the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and computes the difference in conditional indirect effects between any
two rows, that is, between levels of the moderator, or two sets of
levels of the moderators when the path has more than one moderator.

The difference is meaningful when the difference between the two levels
or sets of levels are meaningful. For example, if the two levels are the
mean of the moderator and one standard deviation above mean of the
moderator, then this difference is the change in indirect effect when
the moderator increases by one standard deviation.

If the two levels are 0 and 1, then this difference is the index of
moderated mediation as proposed by Hayes (2015). (This index can also be
computed directly by
[`index_of_mome()`](https://sfcheung.github.io/manymome/reference/index_of_mome.md),
designed specifically for this purpose.)

The function can also compute the change in the standardized indirect
effect between two levels of a moderator or two sets of levels of the
moderators.

This function is intended to be a general purpose function that allows
users to compute the difference between any two levels or sets of levels
that are meaningful in a context.

This function itself does not set the levels of comparison. The levels
to be compared need to be set when calling
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
This function extracts required information from the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

If bootstrap or Monte Carlo estimates are available in the input or
bootstrap or Monte Carlo confidence intervals are requested in calling
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
`cond_indirect_diff()` will also form the bootstrap confidence interval
for the difference in conditional indirect effects using the stored
estimates.

If bootstrap confidence interval is to be formed and both effects used
the same type of interval, then that type will be used. Otherwise,
percentile confidence interval will be formed.

## Functions

- `cond_indirect_diff()`: Compute the difference in in conditional
  indirect effect between two rows in the output of
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## References

Hayes, A. F. (2015). An index and test of linear moderated mediation.
*Multivariate Behavioral Research, 50*(1), 1-22.
[doi:10.1080/00273171.2014.962683](https://doi.org/10.1080/00273171.2014.962683)

## See also

[`index_of_mome()`](https://sfcheung.github.io/manymome/reference/index_of_mome.md)
for computing the index of moderated mediation,
[`index_of_momome()`](https://sfcheung.github.io/manymome/reference/index_of_mome.md)
for computing the index of moderated moderated mediation,
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md),
and
[`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)
for preparing the levels to be compared.

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
dat$xw1 <- dat$x * dat$w1
mod <-
"
m1 ~ a * x  + f * w1 + d * xw1
y  ~ b * m1 + cp * x
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Create levels of w1, the moderators
w1levels <- mod_levels("w1", fit = fit)
w1levels
#>                 w1
#> M+1.0SD  1.2280576
#> Mean     0.2589999
#> M-1.0SD -0.7100578

# Conditional effects from x to y when w1 is equal to each of the levels
boot_out <- fit2boot_out_do_boot(fit, R = 40, seed = 4314, progress = FALSE)
out <- cond_indirect_effects(x = "x", y = "y", m = "m1",
                             wlevels = w1levels, fit = fit,
                             boot_ci = TRUE, boot_out = boot_out)
out
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> y
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>      [w1]   (w1)   ind  CI.lo CI.hi Sig  m1~x  y~m1
#> 1 M+1.0SD  1.228 0.068 -0.776 0.534     0.750 0.091
#> 2 Mean     0.259 0.048 -0.522 0.377     0.523 0.091
#> 3 M-1.0SD -0.710 0.027 -0.267 0.221     0.297 0.091
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 40 samples.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m1~x’,‘y~m1’ is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
#> 
out_ind <- cond_indirect_diff(out, from = 2, to = 1)
out_ind
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> y
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>      [w1]  (w1)   ind  CI.lo CI.hi Sig  m1~x  y~m1
#> 1 M+1.0SD 1.228 0.068 -0.776 0.534     0.750 0.091
#> 2 Mean    0.259 0.048 -0.522 0.377     0.523 0.091
#> 
#> == Difference in Conditional Indirect Effect ==
#> 
#> Levels: 
#>               w1   
#> To:   M+1.0SD 1.228
#> From: Mean    0.259
#> 
#> Levels compared: Row 1 - Row 2
#> 
#> Change in Indirect Effect:
#> 
#>        x y Change  CI.lo CI.hi
#> Change x y  0.021 -0.255 0.166
#> 
#>  - [CI.lo, CI.hi]: 95% percentile confidence interval.
#> 
coef(out_ind)
#>    y~m1~x 
#> 0.0206078 
confint(out_ind)
#>             2.50%    97.50%
#> y~m1~x -0.2547857 0.1662861


```
