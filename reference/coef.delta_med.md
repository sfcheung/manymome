# Delta_Med in a 'delta_med'-Class Object

Return the estimate of Delta_Med in a 'delta_med'-class object.

## Usage

``` r
# S3 method for class 'delta_med'
coef(object, ...)
```

## Arguments

- object:

  The output of
  [`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md).

- ...:

  Optional arguments. Ignored.

## Value

A scalar: The estimate of Delta_Med.

## Details

It just extracts and returns the element `delta_med` in the output of
[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md),
the estimate of the Delta_Med proposed by Liu, Yuan, and Li (2023), an
\\R^2\\-like measure of indirect effect.

## References

Liu, H., Yuan, K.-H., & Li, H. (2023). A systematic framework for
defining R-squared measures in mediation analysis. *Psychological
Methods*. Advance online publication. https://doi.org/10.1037/met0000571

## See also

[`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
library(lavaan)
dat <- data_med
mod <-
"
m ~ x
y ~ m + x
"
fit <- sem(mod, dat)
dm <- delta_med(x = "x",
                y = "y",
                m = "m",
                fit = fit)
dm
#> Call:
#> delta_med(x = "x", y = "y", m = "m", fit = fit)
#> 
#> Predictor (x)       : x 
#> Mediator(s) (m)     : m 
#> Outcome variable (y): y 
#> 
#> Delta_med: 0.230
#> 
#> Paths removed:
#>  m~x
print(dm, full = TRUE)
#> Call:
#> delta_med(x = "x", y = "y", m = "m", fit = fit)
#> 
#> Predictor (x)       : x 
#> Mediator(s) (m)     : m 
#> Outcome variable (y): y 
#> 
#> Delta_med: 0.230
#> 
#> Paths removed:
#>  m~x
#> 
#> Additional information:
#> R-sq: Original                            : 0.351
#> R-sq: Mediator(s) removed                 : 0.121
#> Variance of y                             : 6.273
#> Variance of predicted y                   : 2.203
#> Variance of predicted: mediator(s) removed: 0.759
coef(dm)
#> [1] 0.2302121
```
