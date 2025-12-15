# Create Dummy Variables

Create dummy variables from a categorical variable.

## Usage

``` r
factor2var(
  x_value,
  x_contrasts = "contr.treatment",
  prefix = "",
  add_rownames = TRUE
)
```

## Arguments

- x_value:

  The vector of categorical variable.

- x_contrasts:

  The contrast to be used. Default is `"constr.treatment"`.

- prefix:

  The prefix to be added to the variables to be created. Default is
  `""`.

- add_rownames:

  Whether row names will be added to the output. Default is `TRUE`.

## Value

It always returns a matrix with the number of rows equal to the length
of the vector (`x_value`). If the categorical has only two categories
and so only one dummy variable is needed, the output is still a
one-column "matrix" in R.

## Details

Its main use is for creating dummy variables (indicator variables) from
a categorical variable, to be used in
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

Optionally, the other contrasts can be used through the argument
`x_contrasts`.

## Examples

``` r
dat <- data_mod_cat
dat <- data.frame(dat,
                  factor2var(dat$w, prefix = "gp", add_rownames = FALSE))
head(dat[, c("w", "gpgroup2", "gpgroup3")], 15)
#>         w gpgroup2 gpgroup3
#> 1  group3        0        1
#> 2  group3        0        1
#> 3  group2        1        0
#> 4  group2        1        0
#> 5  group1        0        0
#> 6  group2        1        0
#> 7  group3        0        1
#> 8  group2        1        0
#> 9  group1        0        0
#> 10 group2        1        0
#> 11 group1        0        0
#> 12 group3        0        1
#> 13 group3        0        1
#> 14 group2        1        0
#> 15 group3        0        1
```
