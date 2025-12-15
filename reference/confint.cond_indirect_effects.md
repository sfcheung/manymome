# Confidence Intervals of Indirect Effects or Conditional Indirect Effects

Return the confidence intervals of the conditional indirect effects or
conditional effects in the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'cond_indirect_effects'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  The output of
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- parm:

  Ignored. Always returns the confidence intervals of the effects for
  all levels stored.

- level:

  The level of confidence, default is .95, returning the 95% confidence
  interval. Ignored for now and will use the level of the stored
  intervals.

- ...:

  Additional arguments. Ignored by the function.

## Value

A data frame with two columns, one for each confidence limit of the
confidence intervals. The number of rows is equal to the number of rows
of `object`.

## Details

It extracts and returns the columns for confidence intervals, if
available.

The type of confidence intervals depends on the call used to compute the
effects. If confidence intervals have already been formed (e.g., by
bootstrapping or Monte Carlo), then this function merely retrieves the
confidence intervals stored.

If the following conditions are met, the stored standard errors, if
available, will be used test an effect and form it confidence interval:

- Confidence intervals have not been formed (e.g., by bootstrapping or
  Monte Carlo).

- The path has no mediators.

- The model has only one group.

- The path is moderated by one or more moderator.

- Both the `x`-variable and the `y`-variable are not standardized.

If the model is fitted by OLS regression (e.g., using
[`stats::lm()`](https://rdrr.io/r/stats/lm.html)), then the
variance-covariance matrix of the coefficient estimates will be used,
and confidence intervals are computed from the *t* statistic.

If the model is fitted by structural equation modeling using `lavaan`,
then the variance-covariance computed by `lavaan` will be used, and
confidence intervals are computed from the *z* statistic.

### Caution

If the model is fitted by structural equation modeling and has
moderators, the standard errors, *p*-values, and confidence interval
computed from the variance-covariance matrices for conditional effects
can only be trusted if all covariances involving the product terms are
free. If any of them are fixed, for example, fixed to zero, it is
possible that the model is not invariant to linear transformation of the
variables.

## See also

[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x  + w1 + x:w1
m2 ~ m1
y  ~ m2 + x + w4 + m2:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Examples for cond_indirect():

# Create levels of w1 and w4
w1levels <- mod_levels("w1", fit = fit)
w1levels
#>                 w1
#> M+1.0SD  1.2280576
#> Mean     0.2589999
#> M-1.0SD -0.7100578
w4levels <- mod_levels("w4", fit = fit)
w4levels
#>                 w4
#> M+1.0SD  1.2087784
#> Mean     0.1532493
#> M-1.0SD -0.9022798
w1w4levels <- merge_mod_levels(w1levels, w4levels)

# Conditional effects from x to m1 when w1 is equal to each of the levels
# R should be at least 2000 or 5000 in real research.
out1 <- suppressWarnings(cond_indirect_effects(x = "x", y = "m1",
                      wlevels = w1levels, fit = fit,
                      boot_ci = TRUE, R = 20, seed = 54151,
                      parallel = FALSE,
                      progress = FALSE))
confint(out1)
#>             2.5 %    97.5 %
#> M+1.0SD 0.5470997 0.9277341
#> Mean    0.4277303 0.5787753
#> M-1.0SD 0.1590855 0.4261894

```
