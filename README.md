# manymome

Moderation, mediation, and moderated mediation in structural equation modelling (SEM)
and path models fitted by linear regression.

## What It Can Do?

- Compute an unstandardized or standardized indirect effect or
  conditional indirect effect in a path model.

- Form the bootstrap confidence interval for this effect.

## Advantages

- **A Simpler Workflow**
    - No need to create any parameters or similar
  code when
  fitting a model. Just focus on fitting the model first. After a model has
  been selected, users can compute the effect for *any* pathway, from
  *any* variable, to *any* other variables, conditioned on *any* moderators,
  and at *any* levels of the moderators.

- **Supports Both SEM-Based Modelling and Regression-Based**
    - Supports structural equation models fitted by `lavaan::sem()` or by
  path models fitted by regression using `lm()`. The interface of the main
  functions are similar for both approaches.

- **Flexible in the Form of Models**
    - No limit on the number of predictors, mediators, and
  outcome variables, other than that by `lavaan::sem()` and `lm()`.

- **Supports Latent Variables Mediation and Moderated Mediation**
    - Supports indirect effects and conditional indirect effects among
      latent variables for models fitted by `lavaan::sem()`.

- **Supports Missing Data**
    - Supports dataset with missing data
   through `lavaan::sem()` with full information maximum likelihood (`fiml`).

- **Supports Numeric and Categorical Moderators**
    - Supports numeric and
  categorical moderators. Capitalizes on the native support of categorical
  moderators in `lm()` and has a function (`factor2var()`) for the easy
   creation of dummy variables in `lavaan::sem()`.

- **Less Time for Bootstrapping**
    - Bootstrapping, which is time consuming, can
  be conducted just once. The main functions for computing indirect effects
  and indirect effects can be called as many times as needed without redoing
  bootstrapping.

## Limitations (For Now)

- Does not support categorical predictors.

## How To Use It?

- A good starting point is the "Get Started" article (`vignette("manymome")`).

- More articles can be found at [Articles](./articles/index.html)

## Homepage

For more information on this package, please visit its GitHub page:

https://sfcheung.github.io/manymome/

# Installation

Stable release versions of this package can be downloaded below:

https://github.com/sfcheung/manymome/releases

The latest developmental version of this package can be installed by `remotes::install_github`:

```
remotes::install_github("sfcheung/manymome")
```

# Issues

If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue. Thanks:

https://github.com/sfcheung/manymome/issues