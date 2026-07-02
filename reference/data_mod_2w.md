# Sample Dataset: Two Moderators (Version 2)

A two-moderator model, with variation in the standard deviation of `x`
related to one moderator.

## Usage

``` r
data_mod_2w
```

## Format

A data frame with 200 rows and 6 variables:

- y:

  Outcome variable. Numeric.

- x:

  Predictor. Numeric.

- w1:

  Moderator 1. Numeric.

- w2:

  Moderator 2. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_mod_2w)
lm_out <- lm(y ~ x*w1 + x*w2 + c1 + c2, data_mod_2w)
out <- cond_effects(
  wlevels = c("w1", "w2"),
  x = "x",
  fit = lm_out
)
out
#> 
#> == Conditional effects ==
#> 
#>  Path: x -> y
#>  Conditional on moderator(s): w1, w2
#>  Moderator(s) represented by: w1, w2
#> 
#>      [w1]    [w2]  (w1)  (w2)   ind    SE  Stat pvalue Sig  CI.lo CI.hi
#> 1 M+1.0SD M+1.0SD 7.942 4.829 0.528 0.122 4.343  0.000 ***  0.288 0.768
#> 2 M+1.0SD M-1.0SD 7.942 2.897 0.455 0.141 3.220  0.002 **   0.176 0.734
#> 3 M-1.0SD M+1.0SD 5.813 4.829 0.121 0.163 0.744  0.458     -0.201 0.444
#> 4 M-1.0SD M-1.0SD 5.813 2.897 0.048 0.159 0.303  0.762     -0.266 0.363
#> 
#>  - [SE] are regression standard errors.
#>  - [Stat] are the t statistics used to test the effects.
#>  - [pvalue] are p-values computed from 'Stat'.
#>  - [Sig]: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘ ’ 1.
#>  - [CI.lo to CI.hi] are 95.0% confidence interval computed from
#>    regression standard errors.
#>  - The 'ind' column shows the conditional effects.
#>  
plot(out, facet_grid_cols = "w1", graph_type = "tumble")

plot(out, facet_grid_cols = "w2", graph_type = "tumble")
```
