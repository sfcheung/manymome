# Technical Appendix: Workflow of \`get_prod()\` and Friends

## Goal

This technical appendix describes how product terms in a model or
dataset is identified in the package
[manymome](https://cran.r-project.org/package=manymome) ([Cheung &
Cheung, 2024](https://doi.org/10.3758/s13428-023-02224-z)).

The function,
[`get_prod()`](https://sfcheung.github.io/manymome/reference/get_prod.md),
is called internally by other main functions. Users do not need to call
it directly. Nevertheless, advanced users may be interested in learning
how it works.

## `get_prod()`

This function is used to find all product term(s), if any, from `x` to
`y`, `x` having a direct path to `y`.

### Main arguments

- `x`: The name of the predictor.

- `y`: The name of the outcome.

- `fit`: A fit object. Currently only supports a `lavaan`-class object.

- `est`: The output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
  If `NULL`, the default, it will be generated from `fit`. If supplied,
  `fit` will ge ignored.

- `data`: A data frame. If supplied, it will be used to identify the
  product terms.

- `operator`: The string used to indicate a product term. Default is
  `":"`, used in both [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) for
  observed variables. If raw data is not available, this is needed for
  identifying product terms.

### Output

This is a sample output:

``` r
$prod
[1] "x_o_w1"

$b
   x_o_w1
0.2337979

$w
[1] "w1"

$x
[1] "x"

$y
[1] "m1"
```

It returns a list with these elements:

- `prod`: The name(s) of the product term(s) in the model or data. It
  can have more than one element if the effect of `x` on `y` is
  moderated by more than one moderator.

- `b`: The coefficient(s) of the product term(s).

- `w`: The name(s) of the moderator(s).

- `x`: The predictor, `x`.

- `y`: The outcome variable, `y`.

### Sample Data and Model

The sample dataset `modmed_x1m3w4y1` from the package `manymome` will be
used for illustration:

``` r
library(manymome)
dat <- modmed_x1m3w4y1
print(head(dat), digits = 3)
```

    ##        x     w1     w2     w3     w4    m1    m2     m3    y    gp  city
    ## 1  0.192  1.461 -0.280  0.936  1.239 10.20  9.25  9.435 17.2  mars gamma
    ## 2  0.362  1.302 -0.476 -1.494  0.677 10.46  9.05  6.527 12.7  mars alpha
    ## 3 -1.282  0.409  0.751 -0.938 -1.689  6.94 12.19 19.180 11.1 earth alpha
    ## 4 -0.321 -1.296 -0.439 -1.066  1.588  9.78  8.50 -0.168 11.3 earth gamma
    ## 5  1.941  2.409  1.416  1.589 -0.814 11.79 10.18 12.061 15.2 earth sigma
    ## 6 -0.598  0.861  0.918  1.289  1.713  9.36 11.21 16.945 17.7 earth  beta

For illustration, some product terms will be formed manually, and some
product terms will be formed using the `:` operator in `lavaan`:

``` r
dat$x_o_w1 <- dat$x * dat$w1
library(lavaan)
```

    ## This is lavaan 0.6-21
    ## lavaan is FREE software! Please report any bugs.

``` r
mod <-
"
m1 ~ x   + w1 + x_o_w1
m2 ~ m1  + w2 + m1:w2
m3 ~ m2
y  ~ m3  + w4 + m3:w4 + x + w3 + x:w3 + x:w4
"
fit <- sem(model = mod,
           data = dat,
           meanstructure = TRUE,
           fixed.x = FALSE)
```

Fit the model by [`lm()`](https://rdrr.io/r/stats/lm.html):

``` r
lm_m1 <- lm(m1 ~ x*w1, dat)
lm_m2 <- lm(m2 ~ m1*w2, dat)
lm_m3 <- lm(m3 ~ m2, dat)
lm_y <- lm(y ~ m3*w4 + x*w3 + x*w4, dat)
lm_list <- lm2list(lm_m1, lm_m2, lm_m3, lm_y)
lm_est <- manymome:::lm2ptable(lm_list)
```

### Modes

It works in several modes.

#### `fit` only, with raw data

A `fit` object (`lavaan`-class object) only is supplied, which has raw
data stored.

The parameter estimate table, `est`, is extracted from `fit`.

Raw data is extracted from `fit` to identify product terms.

It then returns product terms that involve `x` in predicting `y`, if
any.

Case: A path with a moderator

``` r
get_prod(x = "x", y = "m1", fit = fit)
```

    ## $prod
    ## [1] "x_o_w1"
    ## 
    ## $b
    ##    x_o_w1 
    ## 0.2337967 
    ## 
    ## $w
    ## [1] "w1"
    ## 
    ## $x
    ## [1] "x"
    ## 
    ## $y
    ## [1] "m1"

Case: A path with more than one moderator

``` r
get_prod(x = "x", y = "y", fit = fit)
```

    ## $prod
    ## [1] "x:w3" "x:w4"
    ## 
    ## $b
    ##       x:w3       x:w4 
    ##  0.5576550 -0.4167135 
    ## 
    ## $w
    ## [1] "w3" "w4"
    ## 
    ## $x
    ## [1] "x"
    ## 
    ## $y
    ## [1] "y"

Case: A path without a moderator

``` r
get_prod(x = "m2", y = "m3", fit = fit)
```

    ## [1] NA

Case: `x` does not have a direct path to `y`

``` r
get_prod(x = "m1", y = "m3", fit = fit)
```

    ## [1] NA

#### `est` and `data`

In this mode, raw data supplied directly is used to identify product
terms.

The parameter estimate table `est` is used to determine the form of the
model.

It then returns product term(s) that involve `x` in predicting `y`, if
any.

Case: A path with a moderator

``` r
get_prod(x = "x", y = "m1", est = lm_est$est, data = lm_est$data)
```

    ## $prod
    ## [1] "x:w1"
    ## 
    ## $b
    ##      x:w1 
    ## 0.2337967 
    ## 
    ## $w
    ## [1] "w1"
    ## 
    ## $x
    ## [1] "x"
    ## 
    ## $y
    ## [1] "m1"

Case: A path with more than one moderator

``` r
get_prod(x = "x", y = "y", est = lm_est$est, data = lm_est$data)
```

    ## $prod
    ## [1] "x:w3" "w4:x"
    ## 
    ## $b
    ##       x:w3       w4:x 
    ##  0.5576550 -0.4167135 
    ## 
    ## $w
    ## [1] "w3" "w4"
    ## 
    ## $x
    ## [1] "x"
    ## 
    ## $y
    ## [1] "y"

Case: A path without a moderator

``` r
get_prod(x = "m2", y = "m3", est = lm_est$est, data = lm_est$data)
```

    ## [1] NA

Case: `x` does not have a direct path to `y`

``` r
get_prod(x = "m1", y = "m3", est = lm_est$est, data = lm_est$data)
```

    ## [1] NA

#### `est` only

In this mode, `operator` needs to be set to identify product terms from
the parameter estimate table `est`.

The table from `est` is then used to determine the form of the model.

It then returns product term(s) that involve `x` in predicting `y`, if
any.

This mode is useful when a product term is not formed from the raw data.
For example, when the product term is a product of two latent variables,
or when raw data is not available.

Case: A path with a moderator

``` r
get_prod(x = "x", y = "m1", est = lm_est$est, operator = ":")
```

    ## $prod
    ## [1] "x:w1"
    ## 
    ## $b
    ##      x:w1 
    ## 0.2337967 
    ## 
    ## $w
    ## [1] "w1"
    ## 
    ## $x
    ## [1] "x"
    ## 
    ## $y
    ## [1] "m1"

Case: A path with more than one moderator

``` r
get_prod(x = "x", y = "y", est = lm_est$est, operator = ":")
```

    ## $prod
    ## [1] "x:w3" "w4:x"
    ## 
    ## $b
    ##       x:w3       w4:x 
    ##  0.5576550 -0.4167135 
    ## 
    ## $w
    ## [1] "w3" "w4"
    ## 
    ## $x
    ## [1] "x"
    ## 
    ## $y
    ## [1] "y"

Case: A path without a moderator

``` r
get_prod(x = "m2", y = "m3", est = lm_est$est, operator = ":")
```

    ## [1] NA

Case: `x` does not have a direct path to `y`

``` r
get_prod(x = "m1", y = "m3", est = lm_est$est, operator = ":")
```

    ## [1] NA

### Workflow

Workflow of
[`manymome::get_prod()`](https://sfcheung.github.io/manymome/reference/get_prod.md)

## `find_all_products()` and `find_product()`

### How it works

The function `find_all_products()` is an internal function for
identifying all product terms in a dataset. It requires the raw data to
work. It is called by
[`get_prod()`](https://sfcheung.github.io/manymome/reference/get_prod.md)
when raw data is available.

For each column in a dataset, it calls `find_product()` to check whether
this column is the product of two other columns. If yes,
`find_product()` returns the component column names. It then returns a
named list of the output of `find_product()` for columns of product
term.

The search by `find_product()` is done numerically, checking whether a
column can be formed from the product of any two other columns (missing
data allowed).

This approach is exact and simple. There is no need to rely on any
special naming convention in the model to denote a product term.

`find_all_products()` returns a named list of product term components.

If `expand` = TRUE, it will try to find all lowest order components.
Therefore, a product term of three or more columns can also be
identified.

### Example

We first create a dataset with some product terms.

``` r
library(manymome)
set.seed(63224)
dat <- round(as.data.frame(MASS::mvrnorm(100, rep(0, 5), diag(5))), 3)
head(dat)
```

    ##       V1     V2     V3     V4     V5
    ## 1 -1.291  0.166  0.384  0.270  1.469
    ## 2  0.516  0.497  0.071  1.538 -0.441
    ## 3 -1.262 -0.146 -1.103 -0.031 -0.664
    ## 4  1.586 -0.098  1.241  1.035 -0.004
    ## 5  2.072  0.384 -0.964 -1.068 -0.307
    ## 6 -0.428 -0.187 -0.717  1.519  0.416

``` r
dat$V2V1 <- dat$V2 * dat$V1
dat$V3V5 <- dat$V3 * dat$V5
dat$V1V2V3 <- dat$V1 * dat$V2 * dat$V3
print(head(dat), digits = 3)
```

    ##       V1     V2     V3     V4     V5   V2V1     V3V5  V1V2V3
    ## 1 -1.291  0.166  0.384  0.270  1.469 -0.214  0.56410 -0.0823
    ## 2  0.516  0.497  0.071  1.538 -0.441  0.256 -0.03131  0.0182
    ## 3 -1.262 -0.146 -1.103 -0.031 -0.664  0.184  0.73239 -0.2032
    ## 4  1.586 -0.098  1.241  1.035 -0.004 -0.155 -0.00496 -0.1929
    ## 5  2.072  0.384 -0.964 -1.068 -0.307  0.796  0.29595 -0.7670
    ## 6 -0.428 -0.187 -0.717  1.519  0.416  0.080 -0.29827 -0.0574

``` r
manymome:::find_all_products(dat)
```

    ## $V2V1
    ## [1] "V1" "V2"
    ## 
    ## $V3V5
    ## [1] "V3" "V5"
    ## 
    ## $V1V2V3
    ## [1] "V3" "V1" "V2"

The output is a named list of character vectors. The names are the
column names that are product terms. Each character vector stores the
column names of its components.

## Reference

Cheung, S. F., & Cheung, S.-H. (2024). *manymome*: An R package for
computing the indirect effects, conditional effects, and conditional
indirect effects, standardized or unstandardized, and their bootstrap
confidence intervals, in many (though not all) models. *Behavior
Research Methods, 56*(5), 4862–4882.
<https://doi.org/10.3758/s13428-023-02224-z>
