# Create Levels of Moderators

Create levels of moderators to be used by
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md),
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
mod_levels(
  w,
  fit,
  w_type = c("auto", "numeric", "categorical"),
  w_method = c("sd", "percentile"),
  sd_from_mean = c(-1, 0, 1),
  percentiles = c(0.16, 0.5, 0.84),
  extract_gp_names = TRUE,
  prefix = NULL,
  values = NULL,
  reference_group_label = NULL,
  descending = TRUE
)

mod_levels_list(
  ...,
  fit,
  w_type = "auto",
  w_method = "sd",
  sd_from_mean = NULL,
  percentiles = NULL,
  extract_gp_names = TRUE,
  prefix = NULL,
  descending = TRUE,
  merge = FALSE
)
```

## Arguments

- w:

  Character. The names of the moderator. If the moderator is categorical
  with 3 or more groups, this is the vector of the indicator variables.

- fit:

  The fit object. Can be a
  [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object or a list of [`lm()`](https://rdrr.io/r/stats/lm.html) outputs.
  It can also be a `lavaan.mi` object returned by
  [`lavaan.mi::lavaan.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html)
  or its wrapper, such as
  [`lavaan.mi::sem.mi()`](https://rdrr.io/pkg/lavaan.mi/man/lavaan.mi.html).
  If it is a single model fitted by
  [`lm()`](https://rdrr.io/r/stats/lm.html), it will be automatically
  converted to a list by
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

- w_type:

  Character. Whether the moderator is a `"numeric"` variable or a
  `"categorical"` variable. If `"auto"`, the function will try to
  determine the type automatically.

- w_method:

  Character, either `"sd"` or `"percentile"`. If `"sd"`, the levels are
  defined by the distance from the mean in terms of standard deviation.
  if `"percentile"`, the levels are defined in percentiles.

- sd_from_mean:

  A numeric vector. Specify the distance in standard deviation from the
  mean for each level. Default is `c(-1, 0, 1)` for `mod_levels()`. For
  `mod_levels_list()`, the default is `c(-1, 0, 1)` when there is only
  one moderator, and `c(-1, 1)` when there are more than one moderator.
  Ignored if `w_method` is not equal to `"sd"`.

- percentiles:

  A numeric vector. Specify the percentile (in proportion) for each
  level. Default is `c(.16, .50, .84)` for `mod_levels()`, corresponding
  approximately to one standard deviation below mean, mean, and one
  standard deviation above mean in a normal distribution. For
  `mod_levels_list()`, default is `c(.16, .50, .84)` if there is one
  moderator, and `c(.16, .84)` when there are more than one moderator.
  Ignored if `w_method` is not equal to `"percentile"`.

- extract_gp_names:

  Logical. If `TRUE`, the default, the function will try to determine
  the name of each group from the variable names.

- prefix:

  Character. If `extract_gp_names` is `TRUE` and `prefix` is supplied,
  it will be removed from the variable names to create the group names.
  Default is `NULL`, and the function will try to determine the prefix
  automatically.

- values:

  For numeric moderators, a numeric vector. These are the values to be
  used and will override other options. For categorical moderators, a
  named list of numeric vector, each vector has length equal to the
  number of indicator variables. If the vector is named, the names will
  be used to label the values. For example, if set to
  `list(gp1 = c(0, 0), gp3 = c(0, 1)`, two levels will be returned, one
  named `gp1` with the indicator variables equal to 0 and 0, the other
  named `gp3` with the indicator variables equal to 0 and 1. Default is
  `NULL`.

- reference_group_label:

  For categorical moderator, if the label for the reference group (group
  with all indicators equal to zero) cannot be determined, the default
  label is `"Reference"`. To change it, set `reference_group_label` to
  the desired label. Ignored if `values` is set.

- descending:

  If `TRUE` (default), the rows are sorted in descending order for
  numerical moderators: The highest value on the first row and the
  lowest values on the last row. For user supplied values, the first
  value is on the last row and the last value is on the first row. If
  `FALSE`, the rows are sorted in ascending order.

- ...:

  The names of moderators variables. For a categorical variable, it
  should be a vector of variable names.

- merge:

  If `TRUE`, `mod_levels_list()` will call
  [`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)
  and return the merged levels. Default is `FALSE`.

## Value

`mod_levels()` returns a `wlevels`-class object which is a data frame
with additional attributes about the levels.

`mod_levels_list()` returns a list of `wlevels`-class objects, or a
`wlevels`-class object which is a data frame of the merged levels if
`merge = TRUE`.

## Details

It creates values of a moderator that can be used to compute conditional
effect or conditional indirect effect. By default, for a numeric
moderator, it uses one standard deviation below mean, mean, and one
standard deviation above mean. The percentiles of these three levels in
a normal distribution (16th, 50th, and 84th) can also be used. For
categorical variable, it will simply collect the unique categories in
the data.

The generated levels are then used by
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

If a model has more than one moderator, `mod_levels_list()` can be used
to generate combinations of levels. The output can then passed to
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
to compute the conditional effects or conditional indirect effects for
all the combinations.

## Functions

- `mod_levels()`: Generate levels for one moderator.

- `mod_levels_list()`: Generate levels for several moderators.

## See also

[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
for computing conditional indiret effects;
[`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)
for merging levels of moderators.

## Examples

``` r
library(lavaan)
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
w2_levels <- mod_levels(lm_out, w = "w2")
w2_levels
#>               w2
#> M+1.0SD 4.040487
#> Mean    3.047789
#> M-1.0SD 2.055091
# Indirect effect from x to y through m, at the first levels of w1 and w2
cond_indirect(x = "x", y = "y", m = "m",
              fit = lm_out,
              wvalues = c(w1 = w1_levels$w1[1],
                          w2 = w2_levels$w2[1]))
#> 
#> == Conditional Indirect Effect   ==
#>                                                     
#>  Path:                        x -> m -> y           
#>  Moderators:                  w1, w2                
#>  Conditional Indirect Effect: 0.398                 
#>  When:                        w1 = 6.173, w2 = 4.040
#> 
#> Computation Formula:
#>   (b.m~x + (b.x:w1)*(w1))*(b.y~m + (b.m:w2)*(w2))
#> 
#> Computation:
#>   ((-0.66304) + (0.20389)*(6.17316))*((-0.14996) + (0.20267)*(4.04049))
#> 
#> Coefficients of Component Paths:
#>  Path Conditional Effect Original Coefficient
#>   m~x              0.596               -0.663
#>   y~m              0.669               -0.150
#> 
# Can form the levels based on percentiles
w1_levels2 <- mod_levels(lm_out, w = "w1", w_method = "percentile")
w1_levels2
#>           w1
#> 84% 6.207972
#> 50% 5.215974
#> 16% 3.932444
# Form the levels from a lavaan output
# Compute the product terms before fitting the model
dat$mw2 <- dat$m * dat$w2
mod <-
"
m ~ x + w1 + x:w1 + c1 + c2
y ~ m + x + w1 + w2 + mw2 + c1 + c2
"
fit <- sem(mod, dat, fixed.x = FALSE)
cond_indirect(x = "x", y = "y", m = "m",
              fit = fit,
              wvalues = c(w1 = w1_levels$w1[1],
                          w2 = w2_levels$w2[1]))
#> 
#> == Conditional Indirect Effect   ==
#>                                                     
#>  Path:                        x -> m -> y           
#>  Moderators:                  w1, w2                
#>  Conditional Indirect Effect: 0.398                 
#>  When:                        w1 = 6.173, w2 = 4.040
#> 
#> Computation Formula:
#>   (b.m~x + (b.x:w1)*(w1))*(b.y~m + (b.mw2)*(w2))
#> 
#> Computation:
#>   ((-0.66304) + (0.20389)*(6.17316))*((-0.14996) + (0.20267)*(4.04049))
#> 
#> Coefficients of Component Paths:
#>  Path Conditional Effect Original Coefficient
#>   m~x              0.596               -0.663
#>   y~m              0.669               -0.150
#> 
# Can pass all levels to cond_indirect_effects()
# First merge the levels by merge_mod_levels()
w1w2_levels <- merge_mod_levels(w1_levels, w2_levels)
cond_indirect_effects(x = "x", y = "y", m = "m",
                      fit = fit,
                      wlevels = w1w2_levels)
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w1, w2
#>  Moderator(s) represented by: w1, w2
#> 
#>      [w1]    [w2]  (w1)  (w2)   ind   m~x   y~m
#> 1 M+1.0SD M+1.0SD 6.173 4.040 0.398 0.596 0.669
#> 2 M+1.0SD Mean    6.173 3.048 0.279 0.596 0.468
#> 3 M+1.0SD M-1.0SD 6.173 2.055 0.159 0.596 0.267
#> 4 Mean    M+1.0SD 5.106 4.040 0.253 0.378 0.669
#> 5 Mean    Mean    5.106 3.048 0.177 0.378 0.468
#> 6 Mean    M-1.0SD 5.106 2.055 0.101 0.378 0.267
#> 7 M-1.0SD M+1.0SD 4.038 4.040 0.107 0.160 0.669
#> 8 M-1.0SD Mean    4.038 3.048 0.075 0.160 0.468
#> 9 M-1.0SD M-1.0SD 4.038 2.055 0.043 0.160 0.267
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 




# mod_levels_list() forms a combinations of levels in one call
# It returns a list, by default.
# Form the levels from a list of lm() outputs
# "merge = TRUE" is optional. cond_indirect_effects will merge the levels
# automatically.
w1w2_levels <- mod_levels_list("w1", "w2", fit = fit, merge = TRUE)
w1w2_levels
#>                                w1       w2
#> w1: M+1.0SD; w2: M+1.0SD 6.173157 4.040487
#> w1: M+1.0SD; w2: M-1.0SD 6.173157 2.055091
#> w1: M-1.0SD; w2: M+1.0SD 4.038047 4.040487
#> w1: M-1.0SD; w2: M-1.0SD 4.038047 2.055091
cond_indirect_effects(x = "x", y = "y", m = "m",
                      fit = fit, wlevels = w1w2_levels)
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w1, w2
#>  Moderator(s) represented by: w1, w2
#> 
#>      [w1]    [w2]  (w1)  (w2)   ind   m~x   y~m
#> 1 M+1.0SD M+1.0SD 6.173 4.040 0.398 0.596 0.669
#> 2 M+1.0SD M-1.0SD 6.173 2.055 0.159 0.596 0.267
#> 3 M-1.0SD M+1.0SD 4.038 4.040 0.107 0.160 0.669
#> 4 M-1.0SD M-1.0SD 4.038 2.055 0.043 0.160 0.267
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 
# Can work without merge = TRUE:
w1w2_levels <- mod_levels_list("w1", "w2", fit = fit)
w1w2_levels
#> [[1]]
#>               w1
#> M+1.0SD 6.173157
#> M-1.0SD 4.038047
#> 
#> [[2]]
#>               w2
#> M+1.0SD 4.040487
#> M-1.0SD 2.055091
#> 
cond_indirect_effects(x = "x", y = "y", m = "m",
                      fit = fit, wlevels = w1w2_levels)
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: x -> m -> y
#>  Conditional on moderator(s): w1, w2
#>  Moderator(s) represented by: w1, w2
#> 
#>      [w1]    [w2]  (w1)  (w2)   ind   m~x   y~m
#> 1 M+1.0SD M+1.0SD 6.173 4.040 0.398 0.596 0.669
#> 2 M+1.0SD M-1.0SD 6.173 2.055 0.159 0.596 0.267
#> 3 M-1.0SD M+1.0SD 4.038 4.040 0.107 0.160 0.669
#> 4 M-1.0SD M-1.0SD 4.038 2.055 0.043 0.160 0.267
#> 
#>  - The 'ind' column shows the conditional indirect effects.
#>  - ‘m~x’,‘y~m’ is/are the path coefficient(s) along the path conditional
#>    on the moderator(s).
#> 

```
