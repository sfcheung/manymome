# Extract the Proportion of Effect Mediated

Return the proportion of effect mediated in the output of
[`indirect_proportion()`](https://sfcheung.github.io/manymome/reference/indirect_proportion.md).

## Usage

``` r
# S3 method for class 'indirect_proportion'
coef(object, ...)
```

## Arguments

- object:

  The output of
  [`indirect_proportion()`](https://sfcheung.github.io/manymome/reference/indirect_proportion.md)

- ...:

  Not used.

## Value

A scalar: The proportion of effect mediated.

## Details

It extracts and returns the element `proportion` in the input object.

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
coef(out)
#> x -> m -> y 
#>   0.5909523 
```
