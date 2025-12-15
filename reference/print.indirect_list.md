# Print an 'indirect_list' Class Object

Print the content of the output of
[`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'indirect_list'
print(
  x,
  digits = 3,
  annotation = TRUE,
  pvalue = FALSE,
  pvalue_digits = 3,
  se = FALSE,
  for_each_path = FALSE,
  ...
)
```

## Arguments

- x:

  The output of
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- digits:

  Number of digits to display. Default is 3.

- annotation:

  Logical. Whether the annotation after the table of effects is to be
  printed. Default is `TRUE.`

- pvalue:

  Logical. If `TRUE`, asymmetric *p*-values based on bootstrapping will
  be printed if available.

- pvalue_digits:

  Number of decimal places to display for the *p*-values. Default is 3.

- se:

  Logical. If `TRUE` and confidence intervals are available, the
  standard errors of the estimates are also printed. They are simply the
  standard deviations of the bootstrap estimates or Monte Carlo
  simulated values, depending on the method used to form the confidence
  intervals.

- for_each_path:

  Logical. If `TRUE`, each of the paths will be printed individually,
  using the `print`-method of the output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
  Default is `FALSE`.

- ...:

  Other arguments. If `for_each_path` is `TRUE`, they will be passed to
  the print method of the output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).
  Ignored otherwise.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The `print` method of the `indirect_list`-class object.

If bootstrapping confidence interval was requested, this method has the
option to print a *p*-value computed by the method presented in
Asparouhov and Muthén (2021). Note that this *p*-value is asymmetric
bootstrap *p*-value based on the distribution of the bootstrap
estimates. It is not computed based on the distribution under the null
hypothesis.

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

```
