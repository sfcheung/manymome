# Proportion of Effect Mediated

It computes the proportion of effect mediated along a pathway.

## Usage

``` r
indirect_proportion(x, y, m = NULL, fit = NULL)
```

## Arguments

- x:

  The name of the `x` variable. Must be supplied as a quoted string.

- y:

  The name of the `y` variable. Must be supplied as a quoted string.

- m:

  A vector of the variable names of the mediator(s). The path goes from
  the first mediator successively to the last mediator. Cannot be `NULL`
  for this function.

- fit:

  The fit object. Can be a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object or a list of [`lm()`](https://rdrr.io/r/stats/lm.html) outputs.
  It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).

## Value

An `indirect_proportion` class object. It is a list-like object with
these major elements:

- `proportion`: The proportion of effect mediated.

- `x`: The name of the `x`-variable.

- `y`: The name of the `y`-variable.

- `m`: A character vector of the mediator(s) along a path. The path runs
  from the first element to the last element.

This class has a `print` method and a `coef` method.

## Details

The proportion of effect mediated along a path from `x` to `y` is the
indirect effect along this path divided by the total effect from `x` to
`y` (Alwin & Hauser, 1975). This total effect is equal to the sum of all
indirect effects from `x` to `y` and the direct effect from `x` to `y`.

To ensure that the proportion can indeed be interpreted as a proportion,
this function computes the the proportion only if the signs of all the
indirect and direct effects from `x` to `y` are same (i.e., all effects
positive or all effects negative).

## References

Alwin, D. F., & Hauser, R. M. (1975). The decomposition of effects in
path analysis. *American Sociological Review, 40*(1), 37.
[doi:10.2307/2094445](https://doi.org/10.2307/2094445)

## See also

[`print.indirect_proportion()`](https://sfcheung.github.io/manymome/reference/print.indirect_proportion.md)
for the `print` method, and
[`coef.indirect_proportion()`](https://sfcheung.github.io/manymome/reference/coef.indirect_proportion.md)
for the `coef` method.

## Examples

``` r
library(lavaan)
dat <- data_med
head(dat)
#>           x        m        y       c1       c2
#> 1  9.931992 17.89644 20.73893 1.426513 6.103290
#> 2  8.331493 17.92150 22.91594 2.940388 3.832698
#> 3 10.327471 17.83178 22.14201 3.012678 5.770532
#> 4 11.196969 20.01750 25.05038 3.120056 4.654931
#> 5 11.887811 22.08645 28.47312 4.440018 3.959033
#> 6  8.198297 16.95198 20.73549 2.495083 3.763712
mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit <- sem(mod, dat, fixed.x = FALSE)
out <- indirect_proportion(x = "x",
                           y = "y",
                           m = "m",
                           fit = fit)
out
#> ==== Proportion of Effect Mediated ====
#> 
#> Path:         x -> m -> y 
#> Proportion:   0.591 
#> Indirect Effect: 0.733 
#> Total Effect:     1.241 
#> 
#> Note:
#> Use coef() to extract the proportion.
#> 
#> All indirect path(s):
#> x -> m -> y
```
