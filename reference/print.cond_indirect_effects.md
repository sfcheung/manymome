# Print a 'cond_indirect_effects' Class Object

Print the content of the output of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Usage

``` r
# S3 method for class 'cond_indirect_effects'
print(
  x,
  digits = 3,
  annotation = TRUE,
  pvalue = NULL,
  pvalue_digits = 3,
  se = NULL,
  level = 0.95,
  se_ci = TRUE,
  ...
)

# S3 method for class 'cond_indirect_effects'
as.data.frame(
  x,
  row.names = NULL,
  optional = NULL,
  digits = 3,
  add_sig = TRUE,
  pvalue = NULL,
  pvalue_digits = 3,
  se = NULL,
  level = 0.95,
  se_ci = TRUE,
  to_string = FALSE,
  ...
)
```

## Arguments

- x:

  The output of
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- digits:

  Number of digits to display. Default is 3.

- annotation:

  Logical. Whether the annotation after the table of effects is to be
  printed. Default is `TRUE.`

- pvalue:

  Logical. If `TRUE`, asymmetric *p*-values based on bootstrapping will
  be printed if available. Default to `FALSE` if confidence intervals
  have already computed. Default to `TRUE` if no confidence intervals
  have been computed and the original standard errors are to be used.
  See Details on when the original standard errors will be used by
  default.

- pvalue_digits:

  Number of decimal places to display for the *p*-values. Default is 3.

- se:

  Logical. If `TRUE` and confidence intervals are available, the
  standard errors of the estimates are also printed. They are simply the
  standard deviations of the bootstrap estimates or Monte Carlo
  simulated values, depending on the method used to form the confidence
  intervals. Default to `FALSE` if confidence intervals are available.
  Default to `TRUE` if no confidence intervals have been computed and
  the original standard errors are to be used. See Details on when the
  original standard errors will be used by default.

- level:

  The level of confidence for the confidence intervals computed from the
  original standard errors. Used only for paths without mediators and
  both `x`- and `y`-variables are not standardized.

- se_ci:

  Logical. If `TRUE` and confidence interval has not been computed, the
  function will try to compute them from stored standard errors if the
  original standard errors are to be used. Ignored if confidence
  intervals have already been computed. Default to `TRUE`.

- ...:

  Other arguments. Not used.

- row.names:

  Not used. Included to be compatible with the generic method.

- optional:

  Not used. Included to be compatible with the generic method.

- add_sig:

  Whether a column of significance test results will be added. Default
  is `TRUE`.

- to_string:

  If `TRUE`, numeric columns will be converted to string columns,
  formatted based on `digits` and `pvalue_digits`. For printing. Default
  is `FALSE`.

## Value

The `print`-method returns `x` invisibly. Called for its side effect.

The `as.data.frame`-method returns a data frame with the conditional
effects and confidence intervals (if available), as well as other
columns requested.

## Details

The `print` method of the `cond_indirect_effects`-class object.

If bootstrapping confidence intervals were requested, this method has
the option to print *p*-values computed by the method presented in
Asparouhov and Muthén (2021). Note that these *p*-values are asymmetric
bootstrap *p*-values based on the distribution of the bootstrap
estimates. They not computed based on the distribution under the null
hypothesis.

For a *p*-value of *a*, it means that a 100(1 - *a*)% bootstrapping
confidence interval will have one of its limits equal to 0. A confidence
interval with a higher confidence level will include zero, while a
confidence interval with a lower confidence level will exclude zero.

### Using Original Standard Errors

If these conditions are met, the stored standard errors, if available,
will be used test an effect and form it confidence interval:

- Confidence intervals have not been formed (e.g., by bootstrapping or
  Monte Carlo).

- The path has no mediators.

- The model has only one group.

- The path is moderated by one or more moderator.

- Both the `x`-variable and the `y`-variable are not standardized.

If the model is fitted by OLS regression (e.g., using
[`stats::lm()`](https://rdrr.io/r/stats/lm.html)), then the
variance-covariance matrix of the coefficient estimates will be used,
and the *p*-value and confidence intervals are computed from the *t*
statistic.

If the model is fitted by structural equation modeling using `lavaan`,
then the variance-covariance computed by `lavaan` will be used, and the
*p*-value and confidence intervals are computed from the *z* statistic.

### Caution

If the model is fitted by structural equation modeling and has
moderators, the standard errors, *p*-values, and confidence interval
computed from the variance-covariance matrices for conditional effects
can only be trusted if all covariances involving the product terms are
free. If any of them are fixed, for example, fixed to zero, it is
possible that the model is not invariant to linear transformation of the
variables.

The method
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for
`cond_indirect_effects` objects is used to convert this class of objects
to data frames. Used internally by the print method but can also be used
for getting a data frame with columns such as *p*-values and standard
errors added.

## Functions

- `as.data.frame(cond_indirect_effects)`: The `as.data.frame`-method for
  `cond_indirect_effects` objects. Used internally by the `print`-method
  but can also be used directly.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

## See also

[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

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
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)

# Conditional effects from x to m1 when w1 is equal to each of the default levels
cond_indirect_effects(x = "x", y = "m1",
                      wlevels = "w1", fit = fit)
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
# when w1 is equal to each of the default levels
out <- cond_indirect_effects(x = "x", y = "y", m = "m1",
                      wlevels = "w1", fit = fit)
out
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

print(out, digits = 5)
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> y
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>      [w1]     (w1)      ind    m1~x     y~m1
#> 1 M+1.0SD  1.22806 -0.03147 0.74988 -0.04197
#> 2 Mean     0.25900 -0.02196 0.52332 -0.04197
#> 3 M-1.0SD -0.71006 -0.01246 0.29676 -0.04197
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m1~x’,‘y~m1’ is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
#> 

print(out, annotation = FALSE)
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



# Convert to data frames

as.data.frame(out)
#>      [w1]       (w1)         ind      m1~x        y~m1
#> 1 M+1.0SD  1.2280576 -0.03147387 0.7498826 -0.04197173
#> 2    Mean  0.2589999 -0.02196465 0.5233201 -0.04197173
#> 3 M-1.0SD -0.7100578 -0.01245543 0.2967576 -0.04197173

as.data.frame(out, to_string = TRUE)
#>      [w1]   (w1)    ind  m1~x   y~m1
#> 1 M+1.0SD  1.228 -0.031 0.750 -0.042
#> 2 Mean     0.259 -0.022 0.523 -0.042
#> 3 M-1.0SD -0.710 -0.012 0.297 -0.042
```
