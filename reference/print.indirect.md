# Print an 'indirect' Class Object

Print the content of the output of
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
or
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'indirect'
print(
  x,
  digits = 3,
  pvalue = NULL,
  pvalue_digits = 3,
  se = NULL,
  level = 0.95,
  se_ci = TRUE,
  wrap_computation = TRUE,
  ...
)
```

## Arguments

- x:

  The output of
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  or
  [`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- digits:

  Number of digits to display. Default is 3.

- pvalue:

  Logical. If `TRUE`, asymmetric *p*-values based on bootstrapping will
  be printed if available. Default to `FALSE` if confidence intervals
  have already computed. Default to `TRUE` if no confidence intervals
  have been computed and the original standard errors are to be used.
  See Details on when the original standard errors will be used by
  default. Default is `NULL` and its value determined as stated above.

- pvalue_digits:

  Number of decimal places to display for the *p*-value. Default is 3.

- se:

  Logical. If `TRUE` and confidence interval has been formed, the
  standard error of the estimates are also printed. It is simply the
  standard deviation of the bootstrap estimates or Monte Carlo simulated
  values, depending on the method used to form the confidence intervals.
  Default to `FALSE` if confidence interval has been formed. Default to
  `TRUE` if no confidence interval has been computed and the original
  standard errors are to be used. See Details on when the original
  standard errors will be used by default. Default is `NULL` and its
  value determined as stated above.

- level:

  The level of confidence for the confidence interval computed from the
  original standard errors. Used only for paths without mediators and
  both `x`- and `y`-variables are not standardized.

- se_ci:

  Logical. If `TRUE` and confidence interval has not been computed, the
  function will try to compute them from stored standard error if the
  original standard error is to be used. Ignored if confidence interval
  has already been computed. Default to `TRUE`.

- wrap_computation:

  Logical. If `TRUE`, the default, long computational symbols and values
  will be wrapped to fit to the screen width.

- ...:

  Other arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The `print` method of the `indirect`-class object.

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

We recommend using confidence interval directly. Therefore, *p*-value is
not printed by default. Nevertheless, users who need it can request it
by setting `pvalue` to `TRUE`.

### Using Original Standard Errors

If these conditions are met, the stored standard error, if available,
will be used to test an effect and form it confidence interval:

- Confidence interval has not been formed (e.g., by bootstrapping or
  Monte Carlo).

- The path has no mediators.

- The model has only one group.

- Both the `x`-variable and the `y`-variable are not standardized.

If the model is fitted by OLS regression (e.g., using
[`stats::lm()`](https://rdrr.io/r/stats/lm.html)), then the
variance-covariance matrix of the coefficient estimates will be used,
and the *p*-value and confidence interval are computed from the *t*
statistic.

If the model is fitted by structural equation modeling using `lavaan`,
then the variance-covariance computed by `lavaan` will be used, and the
*p*-value and confidence interval are computed from the *z* statistic.

### Caution

If the model is fitted by structural equation modeling and has
moderators, the standard errors, *p*-values, and confidence interval
computed from the variance-covariance matrices for conditional effects
can only be trusted if all covariances involving the product terms are
free. If any some of them are fixed, for example, fixed to zero, it is
possible that the model is not invariant to linear transformation of the
variables.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

## See also

[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

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
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

indirect_1 <- cond_indirect(x = "x", y = "y",
                            m = c("m1", "m2", "m3"),
                            fit = fit,
                            wvalues = wvalues)
indirect_1
#> 
#> == Conditional Indirect Effect   ==
#>                                                                             
#>  Path:                        x -> m1 -> m2 -> m3 -> y                      
#>  Moderators:                  w1, w2, w3, w4                                
#>  Conditional Indirect Effect: 1.176                                         
#>  When:                        w1 = 5.000, w2 = 4.000, w3 = 2.000, w4 = 3.000
#> 
#> Computation Formula:
#>   (b.m1~x + (b.x:w1)*(w1))*(b.m2~m1 + (b.m1:w2)*(w2))*(b.m3~m2 +
#>   (b.m2:w3)*(w3))*(b.y~m3 + (b.m3:w4)*(w4))
#> 
#> Computation:
#>   ((0.46277) + (0.23380)*(5.00000))*((0.36130) +
#>   (0.13284)*(4.00000))*((0.68691) + (0.10880)*(2.00000))*((0.40487) +
#>   (0.16260)*(3.00000))
#> 
#> Coefficients of Component Paths:
#>   Path Conditional Effect Original Coefficient
#>   m1~x              1.632                0.463
#>  m2~m1              0.893                0.361
#>  m3~m2              0.905                0.687
#>   y~m3              0.893                0.405
#> 

dat <- modmed_x1m3w4y1
mod2 <-
"
m1 ~ a1 * x
m2 ~ a2 * m1
m3 ~ a3 * m2
y  ~ a4 * m3 + x
"
fit2 <- sem(mod2, dat,
            meanstructure = TRUE, fixed.x = FALSE,
            se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

indirect_2 <- indirect_effect(x = "x", y = "y",
                              m = c("m1", "m2", "m3"),
                              fit = fit2)
indirect_2
#> 
#> == Indirect Effect  ==
#>                                           
#>  Path:            x -> m1 -> m2 -> m3 -> y
#>  Indirect Effect: 0.071                   
#> 
#> Computation Formula:
#>   (b.m1~x)*(b.m2~m1)*(b.m3~m2)*(b.y~m3)
#> 
#> Computation:
#>   (0.52252)*(0.39883)*(0.80339)*(0.42610)
#> 
#> Coefficients of Component Paths:
#>   Path Coefficient
#>   m1~x       0.523
#>  m2~m1       0.399
#>  m3~m2       0.803
#>   y~m3       0.426
#> 
print(indirect_2, digits = 5)
#> 
#> == Indirect Effect  ==
#>                                           
#>  Path:            x -> m1 -> m2 -> m3 -> y
#>  Indirect Effect: 0.07134                 
#> 
#> Computation Formula:
#>   (b.m1~x)*(b.m2~m1)*(b.m3~m2)*(b.y~m3)
#> 
#> Computation:
#>   (0.52252)*(0.39883)*(0.80339)*(0.42610)
#> 
#> Coefficients of Component Paths:
#>   Path Coefficient
#>   m1~x     0.52252
#>  m2~m1     0.39883
#>  m3~m2     0.80339
#>   y~m3     0.42610
#> 

```
