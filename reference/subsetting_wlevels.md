# Extraction Methods for a 'wlevels'-class Object

For subsetting a 'wlevels'-class object. Attributes related to the
levels will be preserved if appropriate.

## Usage

``` r
# S3 method for class 'wlevels'
x[i, j, drop = if (missing(i)) TRUE else length(j) == 1]

# S3 method for class 'wlevels'
x[i, j] <- value

# S3 method for class 'wlevels'
x[[i, j]] <- value
```

## Arguments

- x:

  A 'wlevels'-class object.

- i:

  A numeric vector of row number(s), a character vector of row name(s),
  or a logical vector of row(s) to be selected.

- j:

  A numeric vector of column number(s), a character vector of column
  name(s), or a logical vector of column(s) to be selected.

- drop:

  Whether dropping a dimension if it only have one row/column.

- value:

  Ignored.

## Value

A 'wlevels'-class object. See
[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
and
[`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)
for details on this class.

## Details

Customized `[` for 'wlevels'-class objects, to ensure that these
operations work as they would be on a data frame object, while
information specific to a `wlevels`-class object modified correctly.

The assignment methods `[<-` and `[[<-` for `wlevels`-class objects will
raise an error. This class of objects should be created by
[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
or related functions.

Subsetting the output of
[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
is possible but not recommended. It is more reliable to generate the
levels using
[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
and related functions. Nevertheless, there are situations in which
subsetting is preferred.

## See also

[`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md),
[`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md),
and
[`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)

## Examples

``` r
data(data_med_mod_ab)
dat <- data_med_mod_ab
# Form the levels from a list of lm() outputs
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ m*w2 + x + w1 + c1 + c2, dat)
lm_out <- lm2list(lm_m, lm_y)
w1_levels <- mod_levels(lm_out, w = "w1")
w1_levels
#>               w1
#> M+1.0SD 6.173157
#> Mean    5.105602
#> M-1.0SD 4.038047
w1_levels[2, ]
#>            w1
#> Mean 5.105602
w1_levels[c(2, 3), ]
#>               w1
#> Mean    5.105602
#> M-1.0SD 4.038047

dat <- data_med_mod_serial_cat
lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ m1 + x + w1 + c1 + c2, dat)
lm_out <- lm2list(lm_m1, lm_y)
w1gp_levels <- mod_levels(lm_out, w = "w1")
w1gp_levels
#>        w1group2 w1group3
#> group1        0        0
#> group2        1        0
#> group3        0        1
w1gp_levels[2, ]
#>        w1group2 w1group3
#> group2        1        0
w1gp_levels[3, ]
#>        w1group2 w1group3
#> group3        0        1

merged_levels <- merge_mod_levels(w1_levels, w1gp_levels)
merged_levels
#>                               w1 w1group2 w1group3
#> w1: M+1.0SD; w1: group1 6.173157        0        0
#> w1: M+1.0SD; w1: group2 6.173157        1        0
#> w1: M+1.0SD; w1: group3 6.173157        0        1
#> w1: Mean; w1: group1    5.105602        0        0
#> w1: Mean; w1: group2    5.105602        1        0
#> w1: Mean; w1: group3    5.105602        0        1
#> w1: M-1.0SD; w1: group1 4.038047        0        0
#> w1: M-1.0SD; w1: group2 4.038047        1        0
#> w1: M-1.0SD; w1: group3 4.038047        0        1

merged_levels[4:6, ]
#>                            w1 w1group2 w1group3
#> w1: Mean; w1: group1 5.105602        0        0
#> w1: Mean; w1: group2 5.105602        1        0
#> w1: Mean; w1: group3 5.105602        0        1
merged_levels[1:3, c(2, 3)]
#>                         w1group2 w1group3
#> w1: M+1.0SD; w1: group1        0        0
#> w1: M+1.0SD; w1: group2        1        0
#> w1: M+1.0SD; w1: group3        0        1
merged_levels[c(1, 4, 7), 1, drop = FALSE]
#>                               w1
#> w1: M+1.0SD; w1: group1 6.173157
#> w1: Mean; w1: group1    5.105602
#> w1: M-1.0SD; w1: group1 4.038047
```
