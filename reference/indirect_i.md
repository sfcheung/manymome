# Indirect Effect (No Bootstrapping)

It computes an indirect effect, optionally conditional on the value(s)
of moderator(s) if present.

## Usage

``` r
indirect_i(
  x,
  y,
  m = NULL,
  fit = NULL,
  est = NULL,
  implied_stats = NULL,
  wvalues = NULL,
  standardized_x = FALSE,
  standardized_y = FALSE,
  computation_digits = 5,
  prods = NULL,
  get_prods_only = FALSE,
  data = NULL,
  expand = TRUE,
  warn = TRUE,
  allow_mixing_lav_and_obs = TRUE,
  group = NULL,
  est_vcov = NULL,
  df_residual = NULL,
  skip_indicators = TRUE
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

- fit:

  The fit object. Currently only supports
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  objects. Support for lists of
  [`lm()`](https://rdrr.io/r/stats/lm.html) output is implemented by
  high level functions such as
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  and
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
  It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

- est:

  The output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  If `NULL`, the default, it will be generated from `fit`. If supplied,
  `fit` will be ignored.

- implied_stats:

  Implied means, variances, and covariances of observed variables and
  latent variables (if any), of the form of the output of
  [`lavaan::lavInspect()`](https://rdrr.io/pkg/lavaan/man/lavInspect.html)
  with `what` set to `"implied"`, but with means extracted with `what`
  set to `"mean.ov"` and `"mean.lv"`. The standard deviations are
  extracted from this object for standardization. Default is `NULL`, and
  implied statistics will be computed from `fit` if required.

- wvalues:

  A numeric vector of named elements. The names are the variable names
  of the moderators, and the values are the values to which the
  moderators will be set to. Default is `NULL`.

- standardized_x:

  Logical. Whether `x` will be standardized. Default is `FALSE`.

- standardized_y:

  Logical. Whether `y` will be standardized. Default is `FALSE`.

- computation_digits:

  The number of digits in storing the computation in text. Default is 3.

- prods:

  The product terms found. For internal use.

- get_prods_only:

  IF `TRUE`, will quit early and return the product terms found. The
  results can be passed to the `prod` argument when calling this
  function. Default is `FALSE`. For internal use.

- data:

  Data frame (optional). If supplied, it will be used to identify the
  product terms. For internal use.

- expand:

  Whether products of more than two terms will be searched. `TRUE` by
  default. For internal use.

- warn:

  If `TRUE`, the default, the function will warn against possible
  misspecification, such as not setting the value of a moderator which
  moderate one of the component path. Set this to `FALSE` will suppress
  these warnings. Suppress them only when the moderators are omitted
  intentionally.

- allow_mixing_lav_and_obs:

  If `TRUE`, it accepts a path with both latent variables and observed
  variables. Default is `TRUE`.

- group:

  Either the group number as appeared in the
  [`summary()`](https://rdrr.io/r/base/summary.html) or
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  output of an `lavaan`-class object, or the group label as used in the
  `lavaan`-class object. Used only when the number of groups is greater
  than one. Default is NULL.

- est_vcov:

  A list of variance-covariance matrix of estimates, one for each
  response variable (`y`-variable). Used only for models fitted by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) for now. It is used
  to compute the standard error for a path with no mediator, and both
  `x` and `y` are not standardized.

- df_residual:

  A numeric vector of the residual degrees of freedom for the model of
  each response variable (`y`-variable). Used only for models fitted by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) for now. It is used
  to compute the *p*-value and confidence interval for a path with no
  mediator and both `x` and `y` are not standardized.

- skip_indicators:

  Whether observed indicators are skipped from the search for product
  terms. Default is `TRUE`.

## Value

It returns an `indirect`-class object. This class has the following
methods:
[`coef.indirect()`](https://sfcheung.github.io/manymome/reference/coef.indirect.md),
[`print.indirect()`](https://sfcheung.github.io/manymome/reference/print.indirect.md).
The
[`confint.indirect()`](https://sfcheung.github.io/manymome/reference/confint.indirect.md)
method is used only when called by
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
or
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Details

This function is a low-level function called by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
which call this function multiple times if bootstrap confidence interval
is requested.

This function usually should not be used directly. It is exported for
advanced users and developers

## See also

[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
the high level functions that should usually be used.

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE,
           fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

# Compute the conditional indirect effect by indirect_i()
indirect_1 <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                       wvalues = wvalues)

# Manually compute the conditional indirect effect
indirect_2 <- (est[est$label == "a1", "est"] +
                wvalues["w1"] * est[est$label == "d1", "est"]) *
              (est[est$label == "a2", "est"] +
                wvalues["w2"] * est[est$label == "d2", "est"]) *
              (est[est$label == "a3", "est"] +
                wvalues["w3"] * est[est$label == "d3", "est"]) *
              (est[est$label == "a4", "est"] +
                wvalues["w4"] * est[est$label == "d4", "est"])
# They should be the same
coef(indirect_1)
#>      y~x 
#> 1.176091 
indirect_2
#>       w1 
#> 1.176091 
```
