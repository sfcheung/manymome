# Sample Dataset: One Moderator on Two Predictors

A one-moderator model, with moderator affecting the effects of two
predictors.

## Usage

``` r
data_mod_2x1w
```

## Format

A data frame with 200 rows and 6 variables:

- y:

  Outcome variable. Numeric.

- x1:

  Predictor. Numeric.

- x2:

  Predictor. Numeric.

- w:

  Moderator. Numeric.

- c1:

  Control variable. Numeric.

- c2:

  Control variable. Numeric.

## Examples

``` r
library(lavaan)
data(data_mod_2x1w)
lm_out <- lm(y ~ x1*w + x2*w + c1 + c2, data_mod_2x1w)
out1 <- cond_effects(
  wlevels = "w",
  x = "x1",
  fit = lm_out
)
out1
#> 
#> == Conditional effects ==
#> 
#>  Path: x1 -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w]   (w)    ind    SE   Stat pvalue Sig  CI.lo CI.hi
#> 1 M+1.0SD 2.176  0.357 0.069  5.168  0.000 ***  0.221 0.493
#> 2 Mean    1.717  0.148 0.047  3.160  0.002 **   0.055 0.240
#> 3 M-1.0SD 1.258 -0.062 0.065 -0.953  0.342     -0.189 0.066
#> 
#>  - [SE] are regression standard errors.
#>  - [Stat] are the t statistics used to test the effects.
#>  - [pvalue] are p-values computed from 'Stat'.
#>  - [Sig]: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘ ’ 1.
#>  - [CI.lo to CI.hi] are 95.0% confidence interval computed from
#>    regression standard errors.
#>  - The 'ind' column shows the conditional effects.
#>  
plot(out1, graph_type = "tumble")


out2 <- cond_effects(
  wlevels = "w",
  x = "x2",
  fit = lm_out
)
out2
#> 
#> == Conditional effects ==
#> 
#>  Path: x2 -> y
#>  Conditional on moderator(s): w
#>  Moderator(s) represented by: w
#> 
#>       [w]   (w)    ind    SE   Stat pvalue Sig  CI.lo  CI.hi
#> 1 M+1.0SD 2.176  0.336 0.069  4.862  0.000 ***  0.200  0.472
#> 2 Mean    1.717  0.076 0.050  1.505  0.134     -0.024  0.175
#> 3 M-1.0SD 1.258 -0.184 0.055 -3.363  0.001 *** -0.292 -0.076
#> 
#>  - [SE] are regression standard errors.
#>  - [Stat] are the t statistics used to test the effects.
#>  - [pvalue] are p-values computed from 'Stat'.
#>  - [Sig]: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘ ’ 1.
#>  - [CI.lo to CI.hi] are 95.0% confidence interval computed from
#>    regression standard errors.
#>  - The 'ind' column shows the conditional effects.
#>  
plot(out2, graph_type = "tumble")
```
