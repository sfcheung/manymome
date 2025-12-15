# Extraction Methods for 'cond_indirect_effects' Outputs

For subsetting a 'cond_indirect_effects'-class object.

## Usage

``` r
# S3 method for class 'cond_indirect_effects'
x[i, j, drop = if (missing(i)) TRUE else length(j) == 1]
```

## Arguments

- x:

  A 'cond_indirect_effects'-class object.

- i:

  A numeric vector of row number(s), a character vector of row name(s),
  or a logical vector of row(s) to be selected.

- j:

  A numeric vector of column number(s), a character vector of column
  name(s), or a logical vector of column(s) to be selected.

- drop:

  Whether dropping a dimension if it only have one row/column.

## Value

A 'cond_indirect_effects'-class object. See
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for details on this class.

## Details

Customized `[` for 'cond_indirect_effects'-class objects, to ensure that
these operations work as they would be on a data frame object, while
information specific to conditional effects is modified correctly.

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x  + w1 + x:w1
m2 ~ m1
y  ~ m2 + x + w4 + m2:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Examples for cond_indirect():

# Conditional effects from x to m1 when w1 is equal to each of the levels
out1 <- cond_indirect_effects(x = "x", y = "m1",
                      wlevels = "w1", fit = fit)
out1[2, ]
#> 
#> == Conditional effects ==
#> 
#>  Path: x -> m1
#>  Conditional on moderator(s): w1
#>  Moderator(s) represented by: w1
#> 
#>   [w1]  (w1)   ind
#> 1 Mean 0.259 0.523
#> 
#>  - The 'ind' column shows the conditional effects.
#>  

# Conditional Indirect effect from x1 through m1 to y,
# when w1 is equal to each of the levels
out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                      wlevels = c("w1", "w4"), fit = fit)
out2[c(1, 3), ]
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m1 -> m2 -> y
#>  Conditional on moderator(s): w1, w4
#>  Moderator(s) represented by: w1, w4
#> 
#>      [w1]    [w4]   (w1)  (w4)   ind  m1~x m2~m1  y~m2
#> 1 M+1.0SD M+1.0SD  1.228 1.209 0.137 0.750 0.399 0.458
#> 2 M-1.0SD M+1.0SD -0.710 1.209 0.054 0.297 0.399 0.458
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m1~x’,‘m2~m1’,‘y~m2’ is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
#> 
```
