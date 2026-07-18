# Sample Dataset: Mixed Moderators

A two-moderator model, one categorical and one numerical.

## Usage

``` r
data_mod_cat_num_2w
```

## Format

A data frame with 600 rows and 6 variables:

- y:

  Outcome variable. Numeric.

- x:

  Predictor. Numeric.

- w:

  Moderator 2. Numeric.

- city:

  Moderator 1. String: "City A" and "City B".

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
data(data_mod_cat_num_2w)
lm_out <- lm(y ~ x*city*w + c1 + c2, data_mod_cat_num_2w)
out <- cond_effects(
  wlevels = c("city", "w"),
  x = "x",
  fit = lm_out
)
out
#> 
#> == Conditional effects ==
#> 
#>  Path: x -> y
#>  Conditional on moderator(s): city, w
#>  Moderator(s) represented by: cityCity B, w
#> 
#>   [city]     [w] (cityCity B)    (w)    ind    SE   Stat pvalue Sig  CI.lo
#> 1 City A M+1.0SD            0 24.200 -0.076 0.344 -0.221  0.825     -0.754
#> 2 City A M-1.0SD            0 13.353  0.210 0.285  0.738  0.461     -0.352
#> 3 City B M+1.0SD            1 24.200  1.427 0.156  9.170  0.000 ***  1.120
#> 4 City B M-1.0SD            1 13.353  0.131 0.176  0.745  0.457     -0.216
#>   CI.hi
#> 1 0.602
#> 2 0.772
#> 3 1.734
#> 4 0.479
#> 
#>  - [SE] are regression standard errors.
#>  - [Stat] are the t statistics used to test the effects.
#>  - [pvalue] are p-values computed from 'Stat'.
#>  - [Sig]: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘ ’ 1.
#>  - [CI.lo to CI.hi] are 95.0% confidence interval computed from
#>    regression standard errors.
#>  - The 'ind' column shows the conditional effects.
#>  
plot(out, facet_grid_cols = "city", graph_type = "tumble")
```
