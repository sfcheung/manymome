# Sample Dataset: Serial-Parallel Mediation

A mediation model with both serial and parallel components.

## Usage

``` r
data_serial_parallel
```

## Format

A data frame with 100 rows and 7 variables:

- x:

  Predictor. Numeric.

- m11:

  Mediator 1 in Path 1. Numeric.

- m12:

  Mediator 2 in Path 1. Numeric.

- m2:

  Mediator in Path 2. Numeric.

- y:

  Outcome variable. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_serial_parallel)
mod <-
"
m11 ~ a11 * x + c1 + c2
m12 ~ b11 * m11 + x + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b12 * m12 + b2 * m2 + m11 + x + c1 + c2
indirect1 := a11 * b11 * b12
indirect2 := a2 * b2
indirect := a11 * b11 * b12 + a2 * b2
"
fit <- sem(mod, data_serial_parallel,
           meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 4, 8, 11, 12, 34:36), ]
#>          lhs op               rhs     label   est    se     z pvalue ci.lower
#> 1        m11  ~                 x       a11 0.800 0.093 8.566  0.000    0.617
#> 4        m12  ~               m11       b11 0.465 0.092 5.044  0.000    0.284
#> 8         m2  ~                 x        a2 0.435 0.103 4.239  0.000    0.234
#> 11         y  ~               m12       b12 0.519 0.246 2.108  0.035    0.036
#> 12         y  ~                m2        b2 0.839 0.207 4.052  0.000    0.433
#> 34 indirect1 :=       a11*b11*b12 indirect1 0.193 0.102 1.897  0.058   -0.006
#> 35 indirect2 :=             a2*b2 indirect2 0.364 0.124 2.929  0.003    0.121
#> 36  indirect := a11*b11*b12+a2*b2  indirect 0.558 0.161 3.468  0.001    0.242
#>    ci.upper
#> 1     0.983
#> 4     0.646
#> 8     0.635
#> 11    1.002
#> 12    1.244
#> 34    0.393
#> 35    0.608
#> 36    0.873
```
