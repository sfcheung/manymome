# Coefficient Table of an 'indirect_list' Class Object

Create a coefficient table for the point estimates and confidence
intervals (if available) in the output of
[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
indirect_effects_from_list(object, add_sig = TRUE, pvalue = FALSE, se = FALSE)
```

## Arguments

- object:

  The output of
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  or other functions that return an object of the class `indirect_list`.

- add_sig:

  Whether a column of significance test results will be added. Default
  is `TRUE`.

- pvalue:

  Logical. If `TRUE`, asymmetric *p*-values based on bootstrapping will
  be added available. Default is `FALSE`.

- se:

  Logical. If `TRUE` and confidence intervals are available, the
  standard errors of the estimates are also added. They are simply the
  standard deviations of the bootstrap estimates or Monte Carlo
  simulated values, depending on the method used to form the confidence
  intervals.

## Value

A data frame with the indirect effect estimates and confidence intervals
(if available). It also has A string column, `"Sig"`, for \#'
significant test results if `add_sig` is `TRUE` and confidence intervals
are available.

## Details

If bootstrapping confidence interval was requested, this method has the
option to add *p*-values computed by the method presented in Asparouhov
and Muthén (2021). Note that these *p*-values is asymmetric bootstrap
*p*-values based on the distribution of the bootstrap estimates. They
are not computed based on the distribution under the null hypothesis.

For a *p*-value of *a*, it means that a 100(1 - *a*)% bootstrapping
confidence interval will have one of its limits equal to 0. A confidence
interval with a higher confidence level will include zero, while a
confidence interval with a lower confidence level will exclude zero.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

## See also

[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Examples

``` r
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

# Create a data frame of the indirect effect estimates

out_df <- indirect_effects_from_list(out)
out_df
#>                             ind
#> x -> m11 -> m12 -> y 0.19321379
#> x -> m11 -> y        0.16261213
#> x -> m12 -> y        0.05946653
#> x -> m2 -> y         0.36440188

```
