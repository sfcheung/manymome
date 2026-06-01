# Simple Mediation Models Corrected for Measurement Error

## Introduction

(This document is for Version 0.3.4.25 or later, currently on GitHub
only. The new version is scheduled to be available on CRAN in June
2026.)

This article is a brief illustration of how to use the all-in-one
“quick” functions from
[manymome](https://sfcheung.github.io/manymome/index.html) ([Cheung &
Cheung, 2024](https://doi.org/10.3758/s13428-023-02224-z)) to fit a
structural model and compute and test indirect effects in one step.

It is similar to using them on observed scores, such as scaled scores,
as illustrated in [this
article](https://sfcheung.github.io/manymome/articles/q_mediation.md).
However, that approach does not take into account measurement errors,
which can be done if a construct is measured by several indicators.

This article demonstrates how to fit a model and test indirect effects
when some of the constructs are latent variables measured by multiple
indicators, with the measurement error taken into account using
structural equation modeling (SEM).

NOTE: This is part of a series of similar articles with duplicated
sections, each dedicated to one form of models. See a full list of
articles
[here](https://sfcheung.github.io/manymome/articles/index.html#all-in-one-functions).

## Data Set

The sample dataset `data_indicators` will be used in this illustration.

``` r

library(manymome)
dat <- data_indicators
print(round(head(dat), 2))
#>     x_1   x_2   x_3  x_4  m1_1  m1_2  m1_3  m1_4  m2_1  m2_2  m2_3  m2_4  m3_1
#> 1 -0.15  2.39  0.70 0.31  0.30  1.06 -0.23  0.58 -0.16 -0.09  0.38 -0.89  0.58
#> 2 -1.25  0.59 -2.42 1.27 -0.21 -1.86 -1.78 -1.75 -2.32 -0.84 -1.19 -0.50 -1.95
#> 3 -0.02 -0.82 -0.49 0.48  0.85 -0.19  2.17  0.49  1.49  2.05 -0.84 -0.29  2.59
#> 4  0.24 -1.13 -1.27 1.81 -0.50 -0.91 -2.01  0.41  0.30  1.22 -1.10 -0.33  1.86
#> 5  0.08  1.36  0.32 0.16  1.19  0.34  1.48  0.64  0.43  1.90 -1.08  1.33  3.43
#> 6 -2.20 -1.77 -1.38 0.54  0.31 -1.88  0.16  0.20 -2.74 -2.74 -1.36 -1.06 -0.61
#>    m3_2  m3_3  m3_4   y_1   y_2   y_3   y_4  c1_1  c1_2  c1_3  c1_4  c2_1  c2_2
#> 1 -0.12  2.40  2.05 -2.89 -1.33 -1.83  2.11  0.70  0.09 -2.25  0.27 -1.24 -0.60
#> 2 -2.71 -3.50 -2.26 -1.49 -2.13 -0.24  2.34  0.22  0.70 -0.28  1.43 -0.05 -2.23
#> 3  2.80 -0.16  2.28  1.80 -0.56  1.55 -0.90  1.29 -0.08  1.91  1.26 -0.68 -1.04
#> 4  0.95  1.32  1.09 -0.10  0.28 -2.11  0.56 -1.90 -0.61 -0.75 -2.30 -1.80  0.28
#> 5  2.82  2.91  1.56  2.53  1.92  2.62 -2.18  1.51  2.84  2.88  2.42 -0.85 -0.54
#> 6 -0.90  0.52  1.14  0.58 -1.91  0.71  1.20  1.50 -1.79  1.08  0.17 -1.53 -3.94
#>    c2_3  c2_4     x    m1    m2    m3     y    c1    c2
#> 1 -1.69 -1.52  0.66  0.43 -0.19  1.23 -2.04 -0.30 -1.26
#> 2 -2.93 -2.10 -1.09 -1.40 -1.21 -2.60 -1.55  0.52 -1.83
#> 3 -0.89 -0.09 -0.45  0.83  0.60  1.88  0.92  1.09 -0.68
#> 4  0.07  0.62 -0.99 -0.75  0.02  1.31 -0.62 -1.39 -0.21
#> 5 -2.72 -2.63  0.40  0.91  0.64  2.68  2.31  2.41 -1.69
#> 6 -2.48 -0.73 -1.47 -0.30 -1.98  0.04 -0.45  0.24 -2.17
```

For illustration, only the following variables will be used:

- `x_1`, `x_2`, `x_3`, and `x_4`: The indicators of `fx`, the predictor.
  The indicator `x_4` is a reverse items.

- `m1_1`, `m1_2`, `m1_3`, and `m1_4`: The indicators of `fm`, the
  mediator.

- `y_1`, `y_2`, `y_3`, and `y_4`: The indicators of `fy`, the outcome
  variable. The indicator `y_4` is a reverse items.

- `c1_1`, `c1_2`, `c1_3`, and `c1_4`: The indicators of `fc1`, a control
  variable.

- `c2`: An observed control variable. To illustrate mixing latent and
  observed variables in a model.

## Simple Mediation Model

Suppose we would like to fit a simple mediation model with only one
mediator, `fm`, with `fc1` and `c2` as the control variables.

![Simple Mediation Model](q_mediation_sem_indicators_simple-1.png)

Simple Mediation Model

We would like to fit the model above, and compute and test the indirect
effect along the path `fx -> fm -> fy` using nonparametric
bootstrapping.

## Analysis

We can do that in one single step using
[`q_simple_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md):

``` r

out <- q_simple_mediation(
  x = "fx",
  y = "fy",
  m = "fm",
  cov = c("fc1", "c2"),
  indicators = list(
    fx = c("x_1", "x_2", "x_3", "-x_4"),
    fy = c("y_1", "y_2", "y_3", "-y_4"),
    fm = c("m1_1", "m1_2", "m1_3", "m1_4"),
    fc1 = c("c1_1", "c1_2", "c1_3", "c1_4")
  ),
  fit_method = "sem",
  indicator_method = "measurement_model",
  data = dat,
  R = 5000,
  seed = 1234
)
```

These are the arguments:

- `x`: The name of the predictor (“x” variable).

- `y`: The name of the outcome (“y” variable).

- `m`: The name of the mediator (“m” variable).

- `cov`: The name(s) of the control variable(s) (covariate(s)). By
  default, they predict both the mediator and the outcome.

- `indicators`: The indicators for latent variables. It should be a
  named list of variable names. The name of each element is the name of
  the latent variable. If an indicator is a reverse item, add a minus
  sign, `-`, before its name. It will be reverse-coded (multiplied by
  `-1`) before fitting the model. If a variable in `x`, `y`, `m`, or
  `cov` is not one of the latent factor, it is an observed variable.

- `fit_method` and `indicator_method`: To fit a model with the
  measurement part, using the information from `indicators`, set
  `fit_method` to `"sem"` and `indicator_method` to
  `"measurement_model"`

- `data`: The data frame for the model.

- `R`: The number of bootstrap samples. Should be at least 5000 but can
  be larger for stable results.

- `seed`: The seed for the random number generator. Should always be set
  to an integer to make the results reproducible.

The computation may take some time to run, especially if missing data is
presence, due to the processing time combining bootstrapping and
full-information maximum likelihood (FIML), the default way to handle
missing data.

## Results

We can then just print the output:

``` r

out
```

The printout is long. They will be discussed section-by-section below.

These are the main sections of the default results:

### Basic Information

This is the section `Basic Information`:

    #> ===================================================
    #> |                Basic Information                |
    #> ===================================================
    #> 
    #> Predictor(x): fx
    #> Outcome(y): fy
    #> Mediator(s)(m): fm
    #> Model: Simple Mediation Model
    #> Indicators Method: Fit a measurement model
    #> 
    #> The path model fitted:
    #> 
    #> fm ~ fx + fc1 + c2
    #> fy ~ fm + fx + fc1 + c2
    #> fx =~ x_1 + x_2 + x_3 + x_4
    #> fy =~ y_1 + y_2 + y_3 + y_4
    #> fm =~ m1_1 + m1_2 + m1_3 + m1_4
    #> fc1 =~ c1_1 + c1_2 + c1_3 + c1_4 
    #> 
    #> The original number of cases: 600 
    #> The number of cases in the analysis: 600 
    #> Missing data handling: FIML (full information maximum likelihood)

It shows the variables, the models, and the number of cases. It also
shows the measurement model in the model, in `lavaan` syntax:

- The name on the left-hand side of `=~` is the latent variable.

- The name on the right-hand side of `=~`, joined by `+`, are the
  indicators.

When the model is fitted by SEM, the default method to handle missing
data is full-information maximum likelihood (FIML).

### Indicator Information

    #> ===================================================
    #> |              Indicator Information              |
    #> ===================================================
    #> 
    #> The indicators for the following variable(s):
    #> 
    #> fx: x_1, x_2, x_3, -x_4
    #> fy: y_1, y_2, y_3, -y_4
    #> fm: m1_1, m1_2, m1_3, m1_4
    #> fc1: c1_1, c1_2, c1_3, c1_4
    #> 
    #> Note:
    #> - '-' denotes revserse-coded items.
    #> 
    #> The standardized factor loadings:
    #> 
    #> fx: 
    #> Reliability: 0.7223
    #>     Loading
    #> x_1  0.5699
    #> x_2  0.5980
    #> x_3  0.6589
    #> x_4  0.6780
    #> 
    #> fy: 
    #> Reliability: 0.8812
    #>     Loading
    #> y_1  0.8322
    #> y_2  0.7805
    #> y_3  0.8032
    #> y_4  0.8072
    #> 
    #> fm: 
    #> Reliability: 0.7751
    #>      Loading
    #> m1_1  0.6903
    #> m1_2  0.6278
    #> m1_3  0.7205
    #> m1_4  0.6746
    #> 
    #> fc1: 
    #> Reliability: 0.7149
    #>      Loading
    #> c1_1  0.6035
    #> c1_2  0.6589
    #> c1_3  0.5557
    #> c1_4  0.6607
    #> 
    #> Note:
    #> - Revserse-coded items have been reverse-coded when estimating the
    #>   loadings.
    #> - If the loading of an item is negative, its coding (revserse or
    #>   non-reverse) may be incorrect.
    #> - Reliability estimated by omage coefficients.

The section `Indicator Information` shows results related to each latent
factor:

- The indicators for each latent variable.

- The *standardized* factor loadings from the SEM results.

  - Note that, after recoding, all loadings should be positive. If any
    loading is negative, it may be a reverse indicator not specified in
    `indicators`.

- The reliability coefficient for each latent factor, based on the
  factor loadings. Omega coefficient is used, computed by
  [`semTools::compRelSEM()`](https://rdrr.io/pkg/semTools/man/compRelSEM.html).

### Model Fit

The section `Structural Equation Modeling Results` print the results
from `lavaan` for the model.

    #> Model Test User Model:
    #>                                                       
    #>   Test statistic                               121.132
    #>   Degrees of freedom                               112
    #>   P-value (Chi-square)                           0.262
    #> 
    #> Model Test Baseline Model:
    #> 
    #>   Test statistic                              3192.978
    #>   Degrees of freedom                               136
    #>   P-value                                        0.000
    #> 
    #> User Model versus Baseline Model:
    #> 
    #>   Comparative Fit Index (CFI)                    0.997
    #>   Tucker-Lewis Index (TLI)                       0.996
    #> 
    #> Root Mean Square Error of Approximation:
    #> 
    #>   RMSEA                                          0.012
    #>   Confidence interval - lower                    0.000
    #>   Confidence interval - upper                    0.024
    #>   P-value H_0: RMSEA <= 0.050                    1.000
    #>   P-value H_0: RMSEA >= 0.080                    0.000
    #> 
    #> Standardized Root Mean Square Residual:
    #> 
    #>   SRMR                                           0.031

First, results on goodness-of-fit are printed:

- `Model Test User Model` reports the model $`\chi^2`$ and its *df* and
  *p*-value.

- Basic fit measures (CFI, TLI, RMSEA, and SRMR) are also printed.

### Regression Results

The next few sections print the regression coefficients when predicting
the mediator (`fm` in this model) and the outcome variable (`fy` in this
model).

These are the results in predicting `fm`:

    #>  --------------- 
    #>  Predicting fm : 
    #>  --------------- 
    #> 
    #> Model:
    #>  fm ~ fx + fc1 + c2 
    #>             Estimate   CI.lo   CI.hi   betaS Std. Error z value Pr(>|z|)    
    #> (Intercept)   0.0000  0.0000  0.0000    ----     0.0000    ----     ----    
    #> fx            0.6853  0.4826  0.8880  0.4917     0.1034   6.627   <2e-16 ***
    #> fc1           0.0751 -0.0826  0.2328  0.0568     0.0805   0.934     0.35    
    #> c2            0.0446 -0.0469  0.1362  0.0413     0.0467   0.955     0.34    
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #> R-square:  0.2714 
    #> 
    #> Chi-Squared Difference Test for the R-square
    #> 
    #>       Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
    #> fit1 112 30757 31003 121.13                                  
    #> fit0 115 30857 31090 227.23     106.09       3  < 2.2e-16 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #> - BetaS are standardized coefficients with (a) only numeric variables
    #>   standardized and (b) product terms formed after standardization.
    #>   Variable(s) standardized is/are: fm, fx, fc1, c2
    #> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
    #>   computed from the z values and standard errors.

These are the results in predicting `y`:

    #>  --------------- 
    #>  Predicting fy : 
    #>  --------------- 
    #> 
    #> Model:
    #>  fy ~ fm + fx + fc1 + c2 
    #>             Estimate  CI.lo  CI.hi  betaS Std. Error z value Pr(>|z|)    
    #> (Intercept)   0.0000 0.0000 0.0000   ----     0.0000    ----     ----    
    #> fm            0.4808 0.3235 0.6380 0.3410     0.0802   5.993   <2e-16 ***
    #> fx            0.4902 0.2425 0.7379 0.2494     0.1264   3.879   0.0001 ***
    #> fc1           0.3430 0.1497 0.5363 0.1841     0.0986   3.478   0.0005 ***
    #> c2            0.1618 0.0502 0.2735 0.1061     0.0570   2.842   0.0045 ** 
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #> R-square:  0.3896 
    #> 
    #> Chi-Squared Difference Test for the R-square
    #> 
    #>       Df   AIC   BIC  Chisq Chisq diff Df diff Pr(>Chisq)    
    #> fit1 112 30757 31003 121.13                                  
    #> fit0 116 30963 31191 334.68     213.55       4  < 2.2e-16 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #> - BetaS are standardized coefficients with (a) only numeric variables
    #>   standardized and (b) product terms formed after standardization.
    #>   Variable(s) standardized is/are: fy, fm, fx, fc1, c2
    #> - CI.lo and CI.hi are the 95.0% confidence levels of 'Estimate'
    #>   computed from the z values and standard errors.

The results from
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) are
reformatted to make them similar to those by regression analysis.

If desired, the output of
[`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html) can be
extracted from the output by
[`get_fit()`](https://sfcheung.github.io/manymome/reference/q_mediation.md).

### Indirect Effect Results

By default, this section prints the estimated indirect effect,
confidence interval, and asymmetric bootstrap *p*-value.

Four sections will be printed:

- The original indirect effect.

&nbsp;

    #> == Indirect Effect(s) ==
    #> 
    #>                   ind  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fm -> fy 0.3295 0.2106 0.4718 Sig 0.0000 0.0670
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - The 'ind' column shows the indirect effect(s).

- The indirect effect with the predictor (“x”) standardized.

&nbsp;

    #> == Indirect Effect(s) (x-variable(s) Standardized) ==
    #> 
    #>                   std  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fm -> fy 0.2368 0.1532 0.3302 Sig 0.0000 0.0447
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - std: The partially standardized indirect effect(s).
    #>  - x-variable(s) standardized.

- The indirect effect with the outcome (“y”) standardized.

&nbsp;

    #> == Indirect Effect(s) (y-variable(s) Standardized) ==
    #> 
    #>                   std  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fm -> fy 0.2332 0.1507 0.3343 Sig 0.0000 0.0476
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - std: The partially standardized indirect effect(s).
    #>  - y-variable(s) standardized.

- The indirect effect with both `x` and `y` standardized.

&nbsp;

    #> == Indirect Effect(s) (Both x-variable(s) and y-variable(s) Standardized) ==
    #> 
    #>                   std  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fm -> fy 0.1676 0.1091 0.2339 Sig 0.0000 0.0316
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - std: The standardized indirect effect(s).

The indirect effect with both the predictor (“x”) and the outcome (“y”)
standardized, also called the *completely* *standardized* *indirect*
*effect* or simply the *standardized* *indirect* *effect*.

These four versions of the results are printed by default so that users
can select and interpret sections as they see fit.

### Direct Effect Results

The section `Direct Effect Results` prints the estimated direct effect
(from the predictor “x” to the outcome “y”, not mediated), confidence
interval, and asymmetric bootstrap *p*-value.

    #> ===================================================
    #> |              Direct Effect Results              |
    #> ===================================================
    #> 
    #> ----------------------------------------------------------------
    #> 
    #> == Effect(s) ==
    #> 
    #>             ind  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fy 0.4902 0.2557 0.7588 Sig 0.0012 0.1285
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - The 'ind' column shows the effect(s).
    #>  
    #> ----------------------------------------------------------------
    #> 
    #> == Effect(s) (x-variable(s) Standardized) ==
    #> 
    #>             std  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fy 0.3524 0.1841 0.5269 Sig 0.0012 0.0884
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - std: The partially standardized effect(s).
    #>  - x-variable(s) standardized.
    #>  
    #> ----------------------------------------------------------------
    #> 
    #> == Effect(s) (y-variable(s) Standardized) ==
    #> 
    #>             std  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fy 0.3470 0.1828 0.5339 Sig 0.0012 0.0895
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.
    #>  - std: The partially standardized effect(s).
    #>  - y-variable(s) standardized.
    #>  
    #> ----------------------------------------------------------------
    #> 
    #> == Effect(s) (Both x-variable(s) and y-variable(s) Standardized) ==
    #> 
    #>             std  CI.lo  CI.hi Sig pvalue     SE
    #> fx -> fy 0.2494 0.1315 0.3686 Sig 0.0012 0.0611
    #> 
    #>  - [CI.lo to CI.hi] are 95.0% percentile confidence intervals by
    #>    nonparametric bootstrapping with 5000 samples.
    #>  - [pvalue] are asymmetric bootstrap p-values.
    #>  - [SE] are standard errors.

The *z*-test of the direct effect is already available in section
*Structural Equation Modeling Results*. The bootstrap results are
printed in case users prefer using the same confidence interval method
for all the effects.

## Additional Issues

### Customize Control Variables

If the control variables for the regression models are different, we can
set `cov` to a named list. The names are the variables with control
variables, and the element under each name is a character vector of the
control variables.

For example,

- If we set `cov` to `list(m1 = "c1", m2 = "c2", y = c("c1", "c2"))`,
  then only `c1` is included in predicting `m1`, only `c2` is included
  in predicting `m2`, while both `c1` and `c2` are included in
  predicting `y`.

A variable that does not appear in the list does not have control
variables.

### Customize The Printout

The `print` method of the output of the quick mediation functions have
arguments for customizing the output. These are arguments that likely
may be used:

- `digits`: The number of digits after the decimal place for most
  results. Default is 4.

- `pvalue_digits`: The number of digits after the decimal place for
  *p*-values. Default is 4.

See the help page of
[`print.q_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md)
for other arguments.

### Speed and Parallel Processing

By default, parallel processing is used. If this failed for some
reasons, add `parallel` to `FALSE`. It will take longer, sometimes much
long, to run if `parallel` is set to `FALSE`. Therefore, use parallel
processing whenever possible.

### Progress Bar

By default, a progress bar will be displayed when doing bootstrapping.
This can be disabled by adding `progress = FALSE`.

### Workflow

The quick functions are simply functions to do the following tasks
internally:

- Fit all the models by SEM using
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html).

- Call
  [`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)
  to identify all indirect paths.

- Call
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  to compute all indirect effects and form their confidence intervals.

- Call
  [`total_indirect_effect()`](https://sfcheung.github.io/manymome/reference/total_indirect_effect.md)
  to compute the total indirect effect.

Therefore, all the tasks they do can be done manually by the functions
above. These all-in-one functions are developed just as convenient
functions to do all these tasks in one call.

See this
[article](https://sfcheung.github.io/manymome/articles/med_lav.md) for
computing and testing indirect effects for more complicated models.

## Final Remarks

For details on the all-in-one functions, please refer to the help page
of
[`q_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md).
