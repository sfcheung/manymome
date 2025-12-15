# Mediation Models By Regression or SEM

Simple-to-use functions for fitting linear models by regression or
structural equation modeling and testing indirect effects, using just
one function.

## Usage

``` r
q_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  model = NULL,
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

q_simple_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

q_serial_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

q_parallel_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

# S3 method for class 'q_mediation'
print(
  x,
  digits = 4,
  annotation = TRUE,
  pvalue = TRUE,
  pvalue_digits = 4,
  se = TRUE,
  for_each_path = FALSE,
  se_ci = TRUE,
  wrap_computation = TRUE,
  lm_ci = TRUE,
  lm_beta = TRUE,
  lm_ci_level = 0.95,
  sem_style = c("lm", "lavaan"),
  ...
)
```

## Arguments

- x:

  For `q_mediation()`, `q_simple_mediation()`, `q_serial_mediation()`,
  and `q_parallel_mediation()`, it is the name of the predictor. For the
  `print` method of these functions, `x` is the output of these
  functions.

- y:

  The name of the outcome.

- m:

  A character vector of the name(s) of the mediator(s). For a simple
  mediation model, it must has only one name. For serial and parallel
  mediation models, it can have one or more names. For a serial
  mediation models, the direction of the paths go from the first names
  to the last names. For example, `c("m1", "m3", "m4")` denoted that the
  path is `m1 -> m3 -> m4`.

- cov:

  The names of the covariates, if any. If it is a character vector, then
  the outcome (`y`) and all mediators (`m`) regress on all the
  covariates. If it is a named list of character vectors, then the
  covariates in an element predict only the variable with the name of
  this element. For example, `list(m1 = "c1", dv = c("c2", "c3"))`
  indicates that `c1` predicts `"m1"`, while `c2` and `c3` predicts
  `"dv"`. Default is `NULL`, no covariates.

- data:

  The data frame. Note that listwise deletion will be used and only
  cases with no missing data on all variables in the model (e.g., `x`,
  `m`, `y` and `cov`) will be retained.

- boot_ci:

  Logical. Whether bootstrap confidence interval will be formed. Default
  is `TRUE`.

- mc_ci:

  Logical. Whether Monte Carlo confidence interval will be formed.
  Default is `FALSE`. Only supported if `fit_method` is `"sem"` or
  `"lavaan"`.

- level:

  The level of confidence of the confidence interval. Default is .95
  (for 95% confidence intervals).

- R:

  The number of bootstrap samples. Default is 100. Should be set to 5000
  or at least 10000.

- seed:

  The seed for the random number generator. Default is `NULL`. Should
  nearly always be set to an integer to make the results reproducible.

- ci_type:

  The type of confidence intervals to be formed. Can be either `"boot"`
  (bootstrapping) or `"mc"` (Monte Carlo). If not supplied or is `NULL`,
  will check other arguments (e.g, `boot_ci` and `mc_ci`). If supplied,
  will override `boot_ci` and `mc_ci`. If `fit_method` is `"regression"`
  or `"lm"`, then only `"boot"` is supported.

- boot_type:

  The type of the bootstrap confidence intervals. Default is `"perc"`,
  percentile confidence interval. Set `"bc"` for bias-corrected
  confidence interval. Ignored if `ci_type` is not `"boot"`.

- model:

  The type of model. For `q_mediation()`, it can be `"simple"` (simple
  mediation model), `"serial"` (serial mediation model), or `"parallel"`
  (parallel mediation model). It is recommended to call the
  corresponding wrappers directly (`q_simple_mediation()`,
  `q_serial_mediation()`, and `q_parallel_mediation()`) instead of call
  `q_mediation()`.

- fit_method:

  How the model is to be fitted. If set to `"lm"` or `"regression"`,
  linear regression will be used (fitted by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html)). If set to `"sem"`
  or `"lavaan"`, structural equation modeling will be used and the model
  will be fitted by
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html). Default is
  `"lm"`.

- missing:

  If `fit_method` is set to `"sem"` or `"lavaan"`, this argument
  determine how missing data is handled. The default value is `"fiml"`
  and full information maximum likelihood will be used to handle missing
  data. Please refer to
  [lavaan::lavOptions](https://rdrr.io/pkg/lavaan/man/lavOptions.html)
  for other options.

- fixed.x:

  If `fit_method` is set to `"sem"` or `"lavaan"`, this determines
  whether the observed predictors ("x" variables, including control
  variables) are treated as fixed variables or random variables. Default
  is `TRUE`, to mimic the same implicit setting in regression fitted by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

- sem_args:

  If `fit_method` is set to `"sem"` or `"lavaan"`, this is a named list
  of arguments to be passed to
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html). Arguments
  listed here will not override `missing` and `fixed.x`.

- na.action:

  How missing data is handled. Used only when `fit_method` is set to
  `"sem"` or `"lavaan"`. If `"na.pass"`, the default, then all data will
  be passed to `lavaan`, and full information maximum likelihood
  (`fiml`) will be used to handle missing data. If `"na.omit"`, then
  listwise deletion will be used.

- parallel:

  If `TRUE`, default, parallel processing will be used when doing
  bootstrapping.

- ncores:

  Integer. The number of CPU cores to use when `parallel` is `TRUE`.
  Default is the number of non-logical cores minus one (one minimum).
  Will raise an error if greater than the number of cores detected by
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).
  If `ncores` is set, it will override `make_cluster_args` in
  [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md).

- progress:

  Logical. Display progress or not.

- digits:

  Number of digits to display. Default is 4.

- annotation:

  Logical. Whether the annotation after the table of effects is to be
  printed. Default is `TRUE.`

- pvalue:

  Logical. If `TRUE`, asymmetric *p*-values based on bootstrapping will
  be printed if available. Default is `TRUE`.

- pvalue_digits:

  Number of decimal places to display for the *p*-values. Default is 4.

- se:

  Logical. If `TRUE` and confidence intervals are available, the
  standard errors of the estimates are also printed. They are simply the
  standard deviations of the bootstrap estimates. Default is `TRUE`.

- for_each_path:

  Logical. If `TRUE`, each of the paths will be printed individually,
  using the `print`-method of the output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
  Default is `FALSE`.

- se_ci:

  Logical. If `TRUE` and confidence interval has not been computed, the
  function will try to compute them from stored standard error if the
  original standard error is to be used. Ignored if confidence interval
  has already been computed. Default is `TRUE`.

- wrap_computation:

  Logical. If `TRUE`, the default, long computational symbols and values
  will be wrapped to fit to the screen width.

- lm_ci:

  If `TRUE`, when printing the regression results of
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html), confidence interval
  based on *t* statistic and standard error will be computed and added
  to the output. Default is `TRUE`.

- lm_beta:

  If `TRUE`, when printing the regression results of
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html), standardized
  coefficients are computed and included in the printout. Only numeric
  variables will be computed, and any derived terms, such as product
  terms, will be formed *after* standardization. Default is `TRUE`.

- lm_ci_level:

  The level of confidence of the confidence interval. Ignored if `lm_ci`
  is not `TRUE`.

- sem_style:

  How the for the model is to be printed if the model is fitted by
  structural equation modeling (using `lavaan`). Default is `"lm"` and
  the results will be printed in a style similar to that of
  [`summary()`](https://rdrr.io/r/base/summary.html) output of
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html). If `"lavaan"`, the
  results will be printed in usual `lavaan` style.

- ...:

  Other arguments. If `for_each_path` is `TRUE`, they will be passed to
  the print method of the output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
  Ignored otherwise.

## Value

The function `q_mediation()` returns a `q_mediation` class object, with
its `print` method.

The function `q_simple_mediation()` returns a `q_simple_mediation` class
object, which is a subclass of `q_mediation`.

The function `q_serial_mediation()` returns a `q_serial_mediation` class
object, which is a subclass of `q_mediation`.

The function `q_parallel_mediation()` returns a `q_parallel_mediation`
class object, which is a subclass of `q_mediation`.

## Details

The family of "q" (quick) functions are for testing mediation effects in
common models. These functions do the following in one single call:

- Fit the linear models.

- Compute and test all the indirect effects.

They are easy-to-use and are suitable for common models with mediators.
For now, there are "q" functions for these models:

- A simple mediation: One predictor (`x`), one mediator (`m`), one
  outcome (`y`), and optionally some control variables (`cov`ariates)
  (`q_simple_mediation()`)

- A serial mediation model: One predictor (`x`), one or more mediators
  (`m`), one outcome (`y`), and optionally some control variables
  (`cov`ariates). The mediators positioned sequentially between `x` and
  `y` (`q_serial_mediation()`):

  - `x -> m1 -> m2 -> ... -> y`

- A parallel mediation model: One predictor (`x`), one or more mediators
  (`m`), one outcome (`y`), and optionally some control variables
  (`cov`ariates). The mediators positioned in parallel between `x` and
  `y` (`q_parallel_mediation()`):

  - `x -> m1 -> y`

  - `x -> m2 -> y`

  - ...

- An arbitrary mediation model: One predictor (`x`), one or more
  mediators (`m`), one outcome (`y`), and optionally some control
  variables (`cov`ariates). The mediators positioned in an arbitrary
  form between `x` and `y`, as long as there are no feedback loops
  (`q_mediation()`). For example:

  - `x -> m1`

  - `m1 -> m21 -> y`

  - `m1 -> m22 -> y`

  - ...

Users only need to specify the `x`, `m`, and `y` variables, and
covariates or control variables, if any (by `cov`), and the functions
will automatically identify all indirect effects and total effects.

Note that they are *not* intended to be flexible. For more complex
models, it is recommended to fit the models manually, either by
structural equation modelling (e.g.,
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)) or by
regression analysis using
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
[`lmhelprs::many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.html).
See <https://sfcheung.github.io/manymome/articles/med_lm.html> for an
illustration on how to compute and test indirect effects for an
arbitrary mediation model.

### Specifying a Model of an Arbitrary Form

If a custom model is to be estimated, instead of setting `model` to a
name of the form (`"simple"`,`"serial"`, or `"parallel"`), set
`model to the paths between `x`and`y\`. It can take one of the following
two forms:

A character vector, each element a string of a path, with variable names
connected by `"->"` (the spaces are optional):

    c("x -> m11 -> m12 -> y",
      "x -> m2 -> y")

A list of character vectors, each vector is a vector of names
representing a path, going from the first element to the last element:

    list(c("x", "m11, "m12", "y"),
         c("x", "m2", "y")

The two forms above specify the same model.

Paths not included are fixed to zero (i.e., does not "exist" in the
model). A path can be specified more than once if this can enhance
readability. For example:

    c("x1 -> m1 -> m21 -> y1",
      "x1 -> m1 -> m22 -> y1")

The path `"x1 -> m1"` appears twice, to indicate two different pathways
from `x1` to `y1`.

### Workflow

The coefficients of the model can be estimated by one of these two
methods: OLS (ordinary least squares) regression (setting `fit_method`
to `"regression"` or `"lm"`), or path analysis (SEM, structural equation
modeling, by setting `fit_method` to `"sem"` or `"lavaan"`).

#### Regression

This is the workflow of the "q" functions when estimating the
coefficients by regression:

- Do listwise deletion based on all the variables used in the models.

- Generate the regression models based on the variables specified.

- Fit all the models by OLS regression using
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

- Call
  [`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)
  to identify all indirect paths.

- Call
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  to compute all indirect effects and form their confidence intervals.

- Call
  [`total_indirect_effect()`](https://sfcheung.github.io/manymome/reference/total_indirect_effect.md)
  to compute the total indirect effect.

- Return all the results for printing.

The output of the "q" functions have a `print` method for printing all
the major results.

#### Path Analysis

This is the workflow of the "q" functions when estimating the
coefficients by path analysis (SEM):

- By default, cases with missing data only on the mediators and the
  outcome variable will be retained, and full information maximum
  likelihood (FIML) will be used to estimate the coefficients.
  (Controlled by `missing`, default to `"fiml"`) using
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- Generate the SEM (`lavaan`) model syntax based on the model specified.

- Fit the model by path analysis using
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- Call
  [`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)
  to identify all indirect paths.

- Call
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  to compute all indirect effects and form their confidence intervals.

- Call
  [`total_indirect_effect()`](https://sfcheung.github.io/manymome/reference/total_indirect_effect.md)
  to compute the total indirect effect.

- Return all the results for printing.

### Testing the Indirect Effects

Two methods are available for testing the indirect effects:
nonparametric bootstrap confidence intervals (`ci_type` set to `"boot"`)
and Monte Carlo confidence intervals (`ci_type` set to `"mc"`).

If the coefficients are estimated by OLS regression, only nonparametric
bootstrap confidence intervals are supported.

If the coefficients are estimated by path analysis (SEM), then both
methods are supported.

### Printing the Results

The output of the "q" functions have a `print` method for printing all
the major results.

### Notes

#### Flexibility

The "q" functions are designed to be easy to use. They are not designed
to be flexible. For maximum flexibility, fit the models manually and
call functions such as
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
separately. See
<https://sfcheung.github.io/manymome/articles/med_lm.html> for
illustrations.

#### Monte Carlo Confidence Intervals

We do not recommend using Monte Carlo confidence intervals for models
fitted by regression because the covariances between parameter estimates
are assumed to be zero, which may not be the case in some models.
Therefore, the "q" functions do not support Monte Carlo confidence
intervals if OLS regression is used.

## Methods (by generic)

- `print(q_mediation)`: The `print` method of the outputs of
  `q_mediation()`, `q_simple_mediation()`, `q_serial_mediation()`, and
  `q_parallel_mediation()`.

## Functions

- `q_mediation()`: The general "q" function for common mediation models.
  Not to be used directly.

- `q_simple_mediation()`: A wrapper of `q_mediation()` for simple
  mediation models (a model with only one mediator).

- `q_serial_mediation()`: A wrapper of `q_mediation()` for serial
  mediation models.

- `q_parallel_mediation()`: A wrapper of `q_mediation()` for parallel
  mediation models.

## See also

[`lmhelprs::many_lm()`](https://sfcheung.github.io/lmhelprs/reference/many_lm.html)
for fitting several regression models using model syntax,
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for computing and testing a specific path,
[`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)
for identifying all paths in a model,
[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for computing and testing indirect effects along several paths, and
[`total_indirect_effect()`](https://sfcheung.github.io/manymome/reference/total_indirect_effect.md)
for computing and testing the total indirect effects.

## Author

Idea to fit a model by structural equation modeling by Rong wei Sun
<https://orcid.org/0000-0003-0034-1422>, implemented by Shu Fai Cheung
<https://orcid.org/0000-0002-9871-9448>.

## Examples

``` r
# ===== A user-specified mediation model

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

out <- q_mediation(x = "x1",
                   y = "y1",
                   model = c("x1 -> m11 -> m2 -> y1",
                             "x1 -> m12 -> m2 -> y1"),
                   cov = c("c2", "c1"),
                   data = data_med_complicated,
                   R = 40,
                   seed = 1234,
                   parallel = FALSE,
                   progress = FALSE)
# Suppressed printing of p-values due to the small R
# Remove `pvalue = FALSE` when R is large
print(out,
      pvalue = FALSE)
#> 
#> =============== User-Specified Model ===============
#> 
#> Call:
#> 
#> q_mediation(x = "x1", y = "y1", cov = c("c2", "c1"), data = data_med_complicated, 
#>     R = 40, seed = 1234, model = c("x1 -> m11 -> m2 -> y1", "x1 -> m12 -> m2 -> y1"), 
#>     parallel = FALSE, progress = FALSE)
#> 
#> ===================================================
#> |                Basic Information                |
#> ===================================================
#> 
#> Predictor(x): x1
#> Outcome(y): y1
#> Mediator(s)(m): 
#> Model: User-Specified Model
#> 
#> The regression models fitted:
#> 
#> m11 ~ x1 + c2 + c1
#> m2 ~ m11 + m12 + c2 + c1
#> y1 ~ m2 + c2 + c1
#> m12 ~ x1 + c2 + c1
#> 
#> The number of cases included: 100 
#> 
#> ===================================================
#> |               Regression Results                |
#> ===================================================
#> 
#> 
#> Model:
#> m11 ~ x1 + c2 + c1
#> 
#>             Estimate   CI.lo  CI.hi   betaS Std. Error t value Pr(>|t|) Sig
#> (Intercept)   1.8800 -0.4966 4.2566 -0.0000     1.1973  1.5702   0.1197    
#> x1            0.3543  0.1777 0.5309  0.3830     0.0890  3.9819   0.0001 ***
#> c2           -0.0948 -0.2677 0.0781 -0.1020     0.0871 -1.0881   0.2793    
#> c1            0.0756 -0.1230 0.2741  0.0722     0.1000  0.7555   0.4518    
#> Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#> - BetaS are standardized coefficients with (a) only numeric variables
#>   standardized and (b) product terms formed after standardization.
#>   Variable(s) standardized is/are: m11, x1, c2, c1
#> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
#>   computed from the t values and standard errors.
#> R-square = 0.166. Adjusted R-square = 0.140. F(3, 96) = 6.367, p < .001
#> 
#> Model:
#> m2 ~ m11 + m12 + c2 + c1
#> 
#>             Estimate   CI.lo   CI.hi   betaS Std. Error t value Pr(>|t|) Sig
#> (Intercept)   9.0428  6.6202 11.4653 -0.0000     1.2203  7.4104   0.0000 ***
#> m11           0.0170 -0.1790  0.2131  0.0191     0.0988  0.1726   0.8634    
#> m12          -0.0224 -0.2106  0.1658 -0.0291     0.0948 -0.2361   0.8139    
#> c2           -0.0525 -0.2402  0.1351 -0.0633     0.0945 -0.5558   0.5797    
#> c1           -0.1042 -0.2976  0.0893 -0.1115     0.0974 -1.0690   0.2878    
#> Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#> - BetaS are standardized coefficients with (a) only numeric variables
#>   standardized and (b) product terms formed after standardization.
#>   Variable(s) standardized is/are: m2, m11, m12, c2, c1
#> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
#>   computed from the t values and standard errors.
#> R-square = 0.016. Adjusted R-square = -0.025. F(4, 95) = 0.398, p = 0.810
#> 
#> Model:
#> y1 ~ m2 + c2 + c1
#> 
#>             Estimate   CI.lo   CI.hi   betaS Std. Error t value Pr(>|t|) Sig
#> (Intercept)   9.5114  6.7500 12.2728 -0.0000     1.3911  6.8371   0.0000 ***
#> m2           -0.4364 -0.6499 -0.2228 -0.3775     0.1076 -4.0564   0.0001 ***
#> c2           -0.1677 -0.3437  0.0083 -0.1748     0.0887 -1.8909   0.0617   .
#> c1            0.0982 -0.1010  0.2975  0.0909     0.1004  0.9786   0.3303    
#> Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#> - BetaS are standardized coefficients with (a) only numeric variables
#>   standardized and (b) product terms formed after standardization.
#>   Variable(s) standardized is/are: y1, m2, c2, c1
#> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
#>   computed from the t values and standard errors.
#> R-square = 0.182. Adjusted R-square = 0.156. F(3, 96) = 7.104, p < .001
#> 
#> Model:
#> m12 ~ x1 + c2 + c1
#> 
#>             Estimate   CI.lo   CI.hi   betaS Std. Error t value Pr(>|t|) Sig
#> (Intercept)   8.6694  5.9999 11.3389  0.0000     1.3449  6.4464   0.0000 ***
#> x1           -0.0474 -0.2457  0.1510 -0.0441     0.0999 -0.4741   0.6365    
#> c2           -0.4609 -0.6552 -0.2667 -0.4275     0.0978 -4.7112   0.0000 ***
#> c1            0.2317  0.0087  0.4548  0.1908     0.1124  2.0623   0.0419   *
#> Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#> - BetaS are standardized coefficients with (a) only numeric variables
#>   standardized and (b) product terms formed after standardization.
#>   Variable(s) standardized is/are: m12, x1, c2, c1
#> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
#>   computed from the t values and standard errors.
#> R-square = 0.219. Adjusted R-square = 0.195. F(3, 96) = 8.976, p < .001
#> 
#> ===================================================
#> |             Indirect Effect Results             |
#> ===================================================
#> 
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) ==
#> 
#>                           ind   CI.lo  CI.hi Sig     SE
#> x1 -> m11 -> m2 -> y1 -0.0026 -0.0527 0.0799     0.0235
#> x1 -> m12 -> m2 -> y1 -0.0005 -0.0077 0.0082     0.0031
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 40 samples.
#>  - [SE] are standard errors.
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) (x-variable(s) Standardized) ==
#> 
#>                           std   CI.lo  CI.hi Sig     SE
#> x1 -> m11 -> m2 -> y1 -0.0029 -0.0579 0.0777     0.0251
#> x1 -> m12 -> m2 -> y1 -0.0005 -0.0081 0.0081     0.0034
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 40 samples.
#>  - [SE] are standard errors.
#>  - std: The partially standardized indirect effect(s).
#>  - x-variable(s) standardized.
#>  
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) (y-variable(s) Standardized) ==
#> 
#>                           std   CI.lo  CI.hi Sig     SE
#> x1 -> m11 -> m2 -> y1 -0.0025 -0.0506 0.0717     0.0219
#> x1 -> m12 -> m2 -> y1 -0.0004 -0.0068 0.0079     0.0030
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 40 samples.
#>  - [SE] are standard errors.
#>  - std: The partially standardized indirect effect(s).
#>  - y-variable(s) standardized.
#>  
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) (Both x-variable(s) and y-variable(s) Standardized) ==
#> 
#>                           std   CI.lo  CI.hi Sig     SE
#> x1 -> m11 -> m2 -> y1 -0.0028 -0.0556 0.0697     0.0235
#> x1 -> m12 -> m2 -> y1 -0.0005 -0.0072 0.0085     0.0033
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 40 samples.
#>  - [SE] are standard errors.
#>  - std: The standardized indirect effect(s).
#>  


# ===== Simple mediation

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 20,
                          seed = 1234,
                          parallel = FALSE,
                          progress = FALSE)
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
#> Warning: extreme order statistics used as endpoints
# Suppressed printing of p-values due to the small R
# Remove `pvalue = FALSE` when R is large
print(out,
      pvalue = FALSE)
#> 
#> =============== Simple Mediation Model ===============
#> 
#> Call:
#> 
#> q_simple_mediation(x = "x", y = "y", m = "m", cov = c("c2", "c1"), 
#>     data = data_med, R = 20, seed = 1234, parallel = FALSE, progress = FALSE)
#> 
#> ===================================================
#> |                Basic Information                |
#> ===================================================
#> 
#> Predictor(x): x
#> Outcome(y): y
#> Mediator(s)(m): m
#> Model: Simple Mediation Model
#> 
#> The regression models fitted:
#> 
#> m ~ x + c2 + c1
#> y ~ m + x + c2 + c1
#> 
#> The number of cases included: 100 
#> 
#> ===================================================
#> |               Regression Results                |
#> ===================================================
#> 
#> 
#> Model:
#> m ~ x + c2 + c1
#> 
#>             Estimate   CI.lo   CI.hi   betaS Std. Error t value Pr(>|t|) Sig
#> (Intercept)   9.6894  7.8636 11.5152  0.0000     0.9198 10.5344   0.0000 ***
#> x             0.9347  0.7742  1.0951  0.7536     0.0808 11.5632   0.0000 ***
#> c2           -0.1684 -0.3730  0.0361 -0.1070     0.1030 -1.6343   0.1055    
#> c1            0.1978  0.0454  0.3502  0.1676     0.0768  2.5759   0.0115   *
#> Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#> - BetaS are standardized coefficients with (a) only numeric variables
#>   standardized and (b) product terms formed after standardization.
#>   Variable(s) standardized is/are: m, x, c2, c1
#> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
#>   computed from the t values and standard errors.
#> R-square = 0.598. Adjusted R-square = 0.586. F(3, 96) = 47.622, p < .001
#> 
#> Model:
#> y ~ m + x + c2 + c1
#> 
#>             Estimate   CI.lo   CI.hi   betaS Std. Error t value Pr(>|t|) Sig
#> (Intercept)   4.4152 -2.1393 10.9698  0.0000     3.3016  1.3373   0.1843    
#> m             0.7847  0.2894  1.2800  0.4079     0.2495  3.1450   0.0022  **
#> x             0.5077 -0.0992  1.1145  0.2128     0.3057  1.6608   0.1000    
#> c2           -0.1544 -0.6614  0.3526 -0.0510     0.2554 -0.6045   0.5470    
#> c1            0.1405 -0.2448  0.5258  0.0619     0.1941  0.7239   0.4709    
#> Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
#> - BetaS are standardized coefficients with (a) only numeric variables
#>   standardized and (b) product terms formed after standardization.
#>   Variable(s) standardized is/are: y, m, x, c2, c1
#> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
#>   computed from the t values and standard errors.
#> R-square = 0.358. Adjusted R-square = 0.331. F(4, 95) = 13.220, p < .001
#> 
#> ===================================================
#> |             Indirect Effect Results             |
#> ===================================================
#> 
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) ==
#> 
#>                ind  CI.lo  CI.hi Sig     SE
#> x -> m -> y 0.7334 0.3491 1.4949 Sig 0.3152
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - The 'ind' column shows the indirect effect(s).
#>  
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) (x-variable(s) Standardized) ==
#> 
#>                std  CI.lo  CI.hi Sig     SE
#> x -> m -> y 0.7739 0.3592 1.4945 Sig 0.3164
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - std: The partially standardized indirect effect(s).
#>  - x-variable(s) standardized.
#>  
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) (y-variable(s) Standardized) ==
#> 
#>                std  CI.lo  CI.hi Sig     SE
#> x -> m -> y 0.2914 0.1582 0.5429 Sig 0.1123
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - std: The partially standardized indirect effect(s).
#>  - y-variable(s) standardized.
#>  
#> ----------------------------------------------------------------
#> 
#> == Indirect Effect(s) (Both x-variable(s) and y-variable(s) Standardized) ==
#> 
#>                std  CI.lo  CI.hi Sig     SE
#> x -> m -> y 0.3074 0.1565 0.5428 Sig 0.1129
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - std: The standardized indirect effect(s).
#>  
#> ===================================================
#> |              Direct Effect Results              |
#> ===================================================
#> 
#> ----------------------------------------------------------------
#> 
#> == Effect(s) ==
#> 
#>           ind   CI.lo  CI.hi Sig     SE
#> x -> y 0.5077 -0.3415 1.0654     0.4182
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - The 'ind' column shows the effect(s).
#>  
#> ----------------------------------------------------------------
#> 
#> == Effect(s) (x-variable(s) Standardized) ==
#> 
#>           std   CI.lo  CI.hi Sig     SE
#> x -> y 0.5357 -0.3488 0.9939     0.4189
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - std: The partially standardized effect(s).
#>  - x-variable(s) standardized.
#>  
#> ----------------------------------------------------------------
#> 
#> == Effect(s) (y-variable(s) Standardized) ==
#> 
#>           std   CI.lo  CI.hi Sig     SE
#> x -> y 0.2017 -0.1267 0.4128     0.1690
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - std: The partially standardized effect(s).
#>  - y-variable(s) standardized.
#>  
#> ----------------------------------------------------------------
#> 
#> == Effect(s) (Both x-variable(s) and y-variable(s) Standardized) ==
#> 
#>           std   CI.lo  CI.hi Sig     SE
#> x -> y 0.2128 -0.1326 0.3999     0.1696
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 20 samples.
#>  - [SE] are standard errors.
#>  - std: The standardized effect(s).
#>  
#> ===================================================
#> |                      Notes                      |
#> ===================================================
#> 
#> - For reference, the bootstrap confidence interval (and bootstrap
#>   p-value, if requested) of the (unstandardized) direct effect is also
#>   reported. The bootstrap p-value and the OLS t-statistc p-value can be
#>   different.
#> - For the direct effects with either 'x'-variable or 'y'-variable, or
#>   both, standardized, it is recommended to use the bootstrap confidence
#>   intervals, which take into account the sampling error of the sample
#>   standard deviations.

# # Different control variables for m and y
# out <- q_simple_mediation(x = "x",
#                           y = "y",
#                           m = "m",
#                           cov = list(m = "c1",
#                                      y = c("c1", "c2")),
#                           data = data_med,
#                           R = 100,
#                           seed = 1234,
#                           parallel = FALSE,
#                           progress = FALSE)
# out


# ===== Serial mediation

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

# out <- q_serial_mediation(x = "x",
#                           y = "y",
#                           m = c("m1", "m2"),
#                           cov = c("c2", "c1"),
#                           data = data_serial,
#                           R = 40,
#                           seed = 1234,
#                           parallel = FALSE,
#                           progress = FALSE)

# # Suppressed printing of p-values due to the small R
# # Remove `pvalue = FALSE` when R is large
# print(out,
#       pvalue = FALSE)

# # Different control variables for m and y
# out <- q_serial_mediation(x = "x",
#                           y = "y",
#                           m = c("m1", "m2"),
#                           cov = list(m1 = "c1",
#                                      m2 = c("c2", "c1"),
#                                      y = "c2"),
#                           data = data_serial,
#                           R = 100,
#                           seed = 1234,
#                           parallel = FALSE,
#                           progress = FALSE)
# out


# ===== Parallel mediation

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

# out <- q_parallel_mediation(x = "x",
#                             y = "y",
#                             m = c("m1", "m2"),
#                             cov = c("c2", "c1"),
#                             data = data_parallel,
#                             R = 40,
#                             seed = 1234,
#                             parallel = FALSE,
#                             progress = FALSE)
# # Suppressed printing of p-values due to the small R
# # Remove `pvalue = FALSE` when R is large
# print(out,
#       pvalue = FALSE)

# # Different control variables for m and y
# out <- q_parallel_mediation(x = "x",
#                             y = "y",
#                             m = c("m1", "m2"),
#                             cov = list(m1 = "c1",
#                                        m2 = c("c2", "c1"),
#                                        y = "c2"),
#                             data = data_parallel,
#                             R = 100,
#                             seed = 1234,
#                             parallel = FALSE,
#                             progress = FALSE)
# out
```
