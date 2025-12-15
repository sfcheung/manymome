# Sample Dataset: Parallel Mediation

A parallel mediation model.

## Usage

``` r
data_parallel
```

## Format

A data frame with 100 rows and 6 variables:

- x:

  Predictor. Numeric.

- m1:

  Mediator 1. Numeric.

- m2:

  Mediator 2. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_parallel)
mod <-
"
m1 ~ a1 * x + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b2 * m2 + b1 * m1 + x + c1 + c2
indirect1 := a1 * b1
indirect2 := a2 * b2
indirect := a1 * b1 + a2 * b2
"
fit <- sem(mod, data_parallel,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 4, 7, 8, 27:29), ]
#>          lhs op         rhs     label   est    se     z pvalue ci.lower
#> 1         m1  ~           x        a1 0.877 0.112 7.823  0.000    0.657
#> 4         m2  ~           x        a2 0.297 0.108 2.753  0.006    0.086
#> 7          y  ~          m2        b2 0.471 0.190 2.484  0.013    0.099
#> 8          y  ~          m1        b1 0.486 0.182 2.667  0.008    0.129
#> 27 indirect1 :=       a1*b1 indirect1 0.427 0.169 2.524  0.012    0.095
#> 28 indirect2 :=       a2*b2 indirect2 0.140 0.076 1.845  0.065   -0.009
#> 29  indirect := a1*b1+a2*b2  indirect 0.566 0.185 3.058  0.002    0.203
#>    ci.upper
#> 1     1.097
#> 4     0.508
#> 7     0.843
#> 8     0.844
#> 27    0.758
#> 28    0.288
#> 29    0.929
```
