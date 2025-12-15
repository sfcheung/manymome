# Print an 'indirect_proportion'-Class Object

Print the content of an 'indirect_proportion'-class object, the output
of
[`indirect_proportion()`](https://sfcheung.github.io/manymome/reference/indirect_proportion.md).

## Usage

``` r
# S3 method for class 'indirect_proportion'
print(x, digits = 3, annotation = TRUE, ...)
```

## Arguments

- x:

  An 'indirect_proportion'-class object.

- digits:

  Number of digits to display. Default is 3.

- annotation:

  Logical. Whether additional information should be printed. Default is
  `TRUE`.

- ...:

  Optional arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Details

The `print` method of the `indirect_proportion`-class object, which is
produced by
[`indirect_proportion()`](https://sfcheung.github.io/manymome/reference/indirect_proportion.md).
In addition to the proportion of effect mediated, it also prints
additional information such as the path for which the proportion is
computed, and all indirect path(s) from the x-variable to the
y-variable.

To get the proportion as a scalar, use the `coef` method of
`indirect_proportion` objects.

## See also

[`indirect_proportion()`](https://sfcheung.github.io/manymome/reference/indirect_proportion.md)

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
print(out, digits = 5)
#> ==== Proportion of Effect Mediated ====
#> 
#> Path:         x -> m -> y 
#> Proportion:   0.59095 
#> Indirect Effect: 0.73344 
#> Total Effect:     1.24111 
#> 
#> Note:
#> Use coef() to extract the proportion.
#> 
#> All indirect path(s):
#> x -> m -> y

```
