# Latent Variable Moderated Mediation by SAM

## Introduction

This article is a brief illustration of how to use
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
from the package [manymome](https://sfcheung.github.io/manymome/)
(Cheung & Cheung, 2024) to estimate the conditional indirect effects
among latent variables when the model parameters are estimated by the
SAM (structural-after-measurement) method by Rosseel et al. (2025).

## Data Set and Model

This is the sample data set used for illustration:

``` r

library(manymome)
dat <- data_sem_mome
print(head(dat), digits = 3)
#>       x1      x2     x3      x4     w1     w2     w3     w4     m1     m2
#> 1  0.594  0.3010  0.836  0.1931  2.354  0.185  0.851  1.145  0.636  0.528
#> 2 -0.656 -0.0448  0.687  0.0455 -1.838 -0.724 -0.157 -1.078 -1.198 -0.675
#> 3  0.302 -0.0953  0.347 -0.2332  0.744 -0.734  1.855  0.264  0.257 -0.159
#> 4 -2.022 -0.1876 -1.483 -0.6256 -0.519  0.202  0.727  0.382 -0.517 -1.221
#> 5  0.922  0.1873  0.381 -0.0365 -1.779 -0.992 -2.368 -1.089 -0.163 -0.982
#> 6  0.873  1.9889  0.956  1.2211  0.680  0.437  1.750  0.940  1.329  0.573
#>       m3      m4     y1     y2     y3      y4
#> 1  0.280  0.3634 -0.239  0.924  0.168  0.8163
#> 2 -1.124  0.0211  0.408 -1.062 -0.281 -0.0422
#> 3 -0.294 -0.6764 -0.138  0.914  0.177  0.7359
#> 4 -0.340  0.4333 -0.552  0.334 -0.172  0.1154
#> 5 -0.978 -0.0132 -1.268 -1.430 -0.696 -1.5051
#> 6  1.591  0.4417 -0.284 -0.124 -0.351  0.7251
```

This dataset has indicators of the following four latent variables: one
predictor (`fx`), one mediators (`fm`), one outcome variable (`fy`), and
one moderator (`fw`).

Suppose this is the model being fitted:

![plot of chunk mome_sam_draw_model](mome_sam_draw_model-1.png)

plot of chunk mome_sam_draw_model

The path from `fx` to `fm` is moderated by `fw`, the moderator.

If this model is fitted to the scale scores, then a product term `fx:fw`
is used to model the moderation.

If the model is for the latent variables, the new approach, SAM
(structural-after-measurement), presented in Rosseel et al. (2025) can
be used, using the function
[`sam()`](https://rdrr.io/pkg/lavaan/man/sam.html) from `lavaan`. This
is the model syntax:

``` r

mod <- "
# Measurement model:
fx =~ x1 + x2 + x3 + x4
fw =~ w1 + w2 + w3 + w4
fm =~ m1 + m2 + m3 + m4
fy =~ y1 + y2 + y3 + y4

# Structural model:
fm ~ fx + fw + fx:fw
fy ~ fm + fx
"
```

The moderation effect is modelled by `fx:fw`. To fit this model by SAM,
use [`sam()`](https://rdrr.io/pkg/lavaan/man/sam.html) from `lavaan`.

As recommended by Rosseel et al. (2025), nonparametric bootstrapping
will be used to compute the standard errors and form the confidence
intervals.

``` r

fit <- sam(
  model = mod,
  data = data_sem_mome,
  se = "bootstrap",
  bootstrap.args = list(
    R = 2000
  ),
  iseed = 2345,
  parallel = "snow",
  ncpus = 20
)
```

For details on the SAM approach, see Rosseel et al. (2025) and Rosseel &
Loh (2024).

## Generating Bootstrap Estimates (Optional)

Although bootstrapping has already been conducted when calling
[`sam()`](https://rdrr.io/pkg/lavaan/man/sam.html), it is recommended to
call
[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
to compute additional statistics, such as implied variances and
covariances, to be used in other functions in `manymome`. Otherwise,
this step needs to be repeated every time those functions are called.

``` r

boot_out <- do_boot(fit)
```

Because bootstrap estimates have already been stored, only the output of
[`sam()`](https://rdrr.io/pkg/lavaan/man/sam.html) is sufficient.

## Conditional Indirect Effects

We can now use
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
to estimate the indirect effects for different levels of the moderator
(`fw`) and form their bootstrap confidence intervals. Information stored
by
[`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
can be reused. There is no need to repeat the resampling.

Suppose we want to estimate the indirect effect from `fx` to `fy`
through `fm`, conditional on `fw`:

(Refer to
[`vignette("manymome")`](https://sfcheung.github.io/manymome/articles/manymome.md)
and the help page of
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
on the arguments.)

``` r

out_xmy_on_w <- cond_indirect_effects(
  wlevels = "fw",
  x = "fx",
  y = "fy",
  m = "fm",
  fit = fit,
  boot_ci = TRUE,
  boot_out = boot_out
)
out_xmy_on_w
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: fx -> fm -> fy
#>  Conditional on moderator(s): fw
#>  Moderator(s) represented by: fw
#> 
#>      [fw]   (fw)    ind  CI.lo CI.hi Sig  fm~fx fy~fm
#> 1 M+1.0SD  0.834  0.361  0.141 0.568 Sig  0.810 0.446
#> 2 Mean    -0.000  0.179  0.069 0.288 Sig  0.401 0.446
#> 3 M-1.0SD -0.834 -0.003 -0.042 0.034     -0.007 0.446
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 2000 samples.
#>  - The 'ind' column shows the conditional indirect effects.
#>  - 'fm~fx','fy~fm' is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
```

When `fw` is one standard deviation below mean, the indirect effect is
-0.003, with 95% confidence interval \[-0.042, 0.034\].

When `fw` is one standard deviation above mean, the indirect effect is
0.361, with 95% confidence interval \[0.141, 0.568\].

Note that any conditional indirect path in the model can be estimated
this way. There is no limit on the path to be estimated, as long as all
required path coefficients are in the model.
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
will also check whether a path is valid.

## Standardized Conditional Indirect effects

The standardized conditional indirect effect from `fx` to `fy` through
`fm` conditional on `fw` can be estimated by setting `standardized_x`
and `standardized_y` to `TRUE`:

``` r

std_xmy_on_w <- cond_indirect_effects(
  wlevels = "fw",
  x = "fx",
  y = "fy",
  m = "fm",
  fit = fit,
  boot_ci = TRUE,
  boot_out = boot_out,
  standardized_x = TRUE,
  standardized_y = TRUE
)
std_xmy_on_w
#> 
#> == Conditional indirect effects ==
#> 
#>  Path: fx -> fm -> fy
#>  Conditional on moderator(s): fw
#>  Moderator(s) represented by: fw
#> 
#>      [fw]   (fw)    std  CI.lo CI.hi Sig  fm~fx fy~fm    ind
#> 1 M+1.0SD  0.834  0.435  0.170 0.680 Sig  0.810 0.446  0.361
#> 2 Mean    -0.000  0.215  0.083 0.342 Sig  0.401 0.446  0.179
#> 3 M-1.0SD -0.834 -0.004 -0.050 0.041     -0.007 0.446 -0.003
#> 
#>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
#>    nonparametric bootstrapping with 2000 samples.
#>  - std: The standardized conditional indirect effects. 
#>  - ind: The unstandardized conditional indirect effects.
#>  - 'fm~fx','fy~fm' is/are the path coefficient(s) along the path
#>    conditional on the moderator(s).
```

When `fw` is one standard deviation below mean, the standardized
indirect effect is -0.004, with 95% confidence interval \[-0.050,
0.041\].

When `fw` is one standard deviation above mean, the indirect effect is
0.435, with 95% confidence interval \[0.170, 0.680\].

## Reference(s)

Cheung, S. F., & Cheung, S.-H. (2024). *Manymome*: An R package for
computing the indirect effects, conditional effects, and conditional
indirect effects, standardized or unstandardized, and their bootstrap
confidence intervals, in many (though not all) models. *Behavior
Research Methods*, *56*(5), 4862–4882.
<https://doi.org/10.3758/s13428-023-02224-z>

Rosseel, Y., Burghgraeve, E., Loh, W. W., & Schermelleh-Engel, K.
(2025). Structural after measurement (SAM) approaches for accommodating
latent quadratic and interaction effects. *Behavior Research Methods*,
*57*(4), 101. <https://doi.org/10.3758/s13428-024-02532-y>

Rosseel, Y., & Loh, W. W. (2024). A structural after measurement
approach to structural equation modeling. *Psychological Methods*,
*29*(3), 561–588. <https://doi.org/10.1037/met0000503>
