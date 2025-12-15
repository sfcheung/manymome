# Product Terms (if Any) Along a Path

Identify the product term(s), if any, along a path in a model and return
the term(s), with the variables involved and the coefficient(s) of the
term(s).

## Usage

``` r
get_prod(
  x,
  y,
  operator = ":",
  fit = NULL,
  est = NULL,
  data = NULL,
  expand = FALSE,
  skip_indicators = TRUE
)
```

## Arguments

- x:

  Character. Variable name.

- y:

  Character. Variable name.

- operator:

  Character. The string used to indicate a product term. Default is
  `":"`, used in both [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) for
  observed variables.

- fit:

  The fit object. Currently only supports a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object. It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

- est:

  The output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  If `NULL`, the default, it will be generated from `fit`. If supplied,
  `fit` will ge ignored.

- data:

  Data frame (optional). If supplied, it will be used to identify the
  product terms.

- expand:

  Whether products of more than two terms will be searched. `FALSE` by
  default.

- skip_indicators:

  Whether observed indicators are skipped from the search for product
  terms. Default is `TRUE`.

## Value

If at least one product term is found, it returns a list with these
elements:

- `prod`: The names of the product terms found.

- `b`: The coefficients of these product terms.

- `w`: The variable, other than `x`, in each product term.

- `x`: The `x`-variable, that is, where the path starts.

- `y`: The `y`-variable, that is, where the path ends.

It returns `NA` if no product term is found along the path.

## Details

This function is used by several functions in `manymome` to identify
product terms along a path. If possible, this is done by numerically
checking whether a column in a dataset is the product of two other
columns. If not possible (e.g., the "product term" is the "product" of
two latent variables, formed by the products of indicators), then it
requires the user to specify an operator.

The detailed workflow of this function can be found in the article
<https://sfcheung.github.io/manymome/articles/get_prod.html>

This function is not intended to be used by users. It is exported such
that advanced users or developers can use it.

## Examples

``` r
dat <- modmed_x1m3w4y1
library(lavaan)
mod <-
"
m1 ~ x   + w1 + x:w1
m2 ~ m1  + w2 + m1:w2
m3 ~ m2
y  ~ m3  + w4 + m3:w4 + x + w3 + x:w3 + x:w4
"
fit <- sem(model = mod,
           data = dat,
           meanstructure = TRUE,
           fixed.x = FALSE)

# One product term
get_prod(x = "x", y = "m1", fit = fit)
#> $prod
#> [1] "x:w1"
#> 
#> $b
#>      x:w1 
#> 0.2337967 
#> 
#> $w
#> [1] "w1"
#> 
#> $x
#> [1] "x"
#> 
#> $y
#> [1] "m1"
#> 
# Two product terms
get_prod(x = "x", y = "y", fit = fit)
#> $prod
#> [1] "x:w3" "x:w4"
#> 
#> $b
#>       x:w3       x:w4 
#>  0.5576550 -0.4167135 
#> 
#> $w
#> [1] "w3" "w4"
#> 
#> $x
#> [1] "x"
#> 
#> $y
#> [1] "y"
#> 
# No product term
get_prod(x = "m2", y = "m3", fit = fit)
#> [1] NA
```
