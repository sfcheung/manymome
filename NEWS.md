# manymome 0.2.1

## New Features

### Multigroup Models Supported

- Support for mediation paths in
  multigroup models fitted
  by `lavaan` has been added.
  Demonstrations can be found
  in this [article](https://sfcheung.github.io/manymome/articles/med_mg.html)

    - Many functions have been updated to
      work for multigroup models with
      mediators fitted
      by `lavaan`.
      Most common tasks
      support multigroup models.
      For functions that
      support moderators, the group
      variable will be used automatically
      as a moderator.
      Checks will be added to functions
      not yet support multigroup models
      to alert users.

    - For paths moderated in multigroup
      models, only some functions
      (e.g., `cond_indirect_effect()`)
      are supported. However, multigroup
      models with moderators are rare.
      Functions that do not yet support
      multigroup models (e.g,
      `mod_levels()`) will raise an error
      if used on a multigroup model.
      Support may be added in the future.

    - The `+` and `-` operators can now be
      used on different paths because
      they may be
      paths in different groups in
      multigroup models.

    - The `plot`-method of
      `cond_indirect_effects`-class objects
      will be forced to be a tumble graph
      if the lines for different groups
      are to be plotted. In these cases,
      the data within each group will be used,
      including standardization. This
      approach, though leading to results
      different from those in single-group
      model using the group as a moderator,
      makes more sense for multigroup
      models, in which the distribution of
      variables are allowed to be different
      between groups. Since 0.1.14.10,
      by default, the model implied statistics
      are used to determine the means and SDs
      used in drawing the plot. This approach
      is useful when between-group
      equality constraints
      are present.

    - The `plot`-method of
      `cond_indirect_effects`-class objects
      now supports plotting a path that
      involves latent variables. The model
      implied statistics will always be used
      for the latent variables when determining
      the means and SDs. This is useful
      because the group-variable can
      be treated as a moderator by
      `cond_indirect_effects()`. (0.1.14.7)

### Other New Features

- Added `plot_effect_vs_w()`. It
  can plot an effect (direct or indirect)
  against a moderator, using the output
  of `cond_indirect_effects()`. (0.1.14.14 - 0.1.14.15)

- Added `pseudo_johnson_neyman()`. It
  used the pseudo Johnson-Neyman approach
  (Hayes, 2022) to find the value of
  a moderator at which the conditional
  effect (direct or indirect) changes
  from nonsignificant to significant
  (or vice versa), based on the
  confidence interval selected.
   (0.1.14.16)

## Miscellaneous

- If a dataset has a variable which
  is a product of itself and another
  variable (e.g., `x*y == x`),
  `find_products()` will be trapped
  in an infinite loop. This
  "product term" will no longer be
  treated as a "product term."
  (0.1.14.1)

- Bootstrapping and Monte Carlo
  simulation will no longer be run
  once for each path in
  `many_indirect_effects()`. If
  `do_boot()` or `do_mc()` is not used
  first but bootstrapping or Monte
  Carlo confidence intervals are
  requested, this process will be done
  only once, and the estimates will be
  reused by all paths. (0.1.14.9,
  a bug fixed in 0.2.1)

# manymome 0.1.14

## New Features

- The standardizers (`scale_x` and
  `scale_y`) for each bootstrap or
  simulated sample are now stored, such
  that the confidence interval of
  the unstandardized effect can be
  computed even if standardization is
  requested. (0.1.13.2)

## Bug Fixes

- Fixed a nonessential bug with the math
  operator: `indirect_raw`, though not
  used for now, is now computed correctly
  when using `+` and `-`. (0.1.13.1)
- Fixed a minor typo in documentation.
  (0.1.13.3)
- Fixed a minor issue with the print
  method of `delta_med`-class object.
  (0.1.13.4)
- Fixed a bug with using `do_mc()`
  on a model which do not have a mean
  structure, has latent variables,
  and is estimated by multiple
  imputation. Error is no longer raised.
  (0.1.13.5)
- Fixed a few more tests that should
  not be run if suggested packages are
  not installed. (0.1.13.6)
- No longer raises an error for dichotomous
  moderators. (0.1.13.7)

# manymome 0.1.13

## New Features

- Added `delta_med()` for computing
  $\Delta_{Med}$ (Delta_Med), an
  $R^2$-like measure of indirect
  effect proposed by Liu, Yuan, and Li (2023).
  Can form nonparametric bootstrap
  confidence interval for $\Delta_{Med}$. (0.1.12.1, 0.1.12.3)
- Added support for paths with both
  latent and observed variables. (0.1.12.2, 0.1.12.4)

## Miscellaneous

- Updated references. (0.1.13)

# manymome 0.1.12

## New Features

### Can report standard errors (if requested)

- All major print methods of effects support
  printing standard errors (setting
  `se = TRUE`). They are simply the
  standard deviations of the bootstrap
  estimates (if bootstrap confidence
  intervals are requested) or simulated
  estimates (if Monte Carlo confidence
  intervals are requested). They should
  be interpreted with cautions because
  the sampling distribution of the
  effect estimates may not be symmetric. (0.1.11.2)

## Miscellaneous

- Customized `linters`. (0.1.11.1)

- Revised a test to accommodate a
  behavior of MKL when `MASS::mvrnorm()`
  is used to generate pseudo random
  numbers. (0.1.11.4)

- Finalized to 0.1.12. (0.1.12)

## Bug Fixes

- *P*-value were not computed when
  mathematical operations are conducted
  on effects using `+` and `-` before
  version 0.1.11.2. This
  has been fixed. (0.1.11.2)

- `merge_model_matrix()` failed if all
  variables in an `lm()` output is already
  present in merged outputs. Fixed
  in 0.1.11.3. (0.1.11.3)

- `cond_indirect()` did not hide the
  progress when Monte Carlo CIs were
  requested and `do_mc()` was called
  internally. Fixed. It now hides the
  progress if `progress = TRUE`.
  (0.1.11.5)

# manymome 0.1.10

## New Features

### Monte Carlo Confidence Intervals

- Added support for Monte Carlo confidence intervals. (0.1.9.3 to 0.1.9.4)
- Updated some vignettes for Monte Carlo confidence intervals. (0.1.9.5)
- Please refer to [this article](https://sfcheung.github.io/manymome/articles/do_mc.html)
  for an illustration on forming Monte Carlo confidence interval.

### Multiple Imputation

- Added support for models fitted by `runMI()` or `sem.mi()`
  from the `semTools` package using multiple imputation. (0.1.9.8-0.1.9.10)
- Please refer to [this article](https://sfcheung.github.io/manymome/articles/do_mc_lavaan_mi.html)
  for an illustration on forming Monte Carlo confidence interval.

### Can report *p*-value (if requested)

- Some print methods support printing
  asymmetric bootstrap
  *p*-values using the method presented
  in Asparouhov and Muthén (2021) if bootstrapping
  confidence interval is requested. By
  default, *p*-values are not printed. (0.1.9.7)

### Report proportion of effect mediated

- Added `indirect_proportion()` and two methods for its output. (0.1.9.12)

### Others

- Exported `get_prod()` and added [an article](https://sfcheung.github.io/manymome/articles/get_prod.html)
  on its workflow. (0.1.9.13).
- Bootstrapping can handle the `fixed.x` argument as `lavaan` does. (0.1.9.17)

## Bug Fixes

- Fixed `factor2var()` to work (again) for a categorical variable with only two
  levels. (0.1.9.21)

## Miscellaneous

- Updated badges in README.md. (0.1.9.1)
- Updated `pkgdown` site. (0.1.9.2)
- Used a more reliable test for Monte Carlo CIs. (0.1.9.6)
- Updated the logo for readability. (0.1.9.6)
- Fixed an error in `pkgdown` site. (0.1.9.6)
- Added progress bars to `do_mc()`. (0.1.9.11)
- Added `print.mc_out()`, the print-method for `mc_out`-class objects. (0.1.9.14)
- Updated vignettes with package name. (0.1.9.15)
- Fixed typos in NEWS.md. (0.1.9.15)
- Updated `pkgdown` GitHub action for using newer version of mermaid. (0.1.9.15)
- Updated `pkgdown` website to use the new logo and color scheme. (0.1.9.16)
- Modified more tests to accommodate a change in `lavaan`
  on handling random seed. (0.1.9.18)
- No change to the main code. Added a few technical appendices as `pkgdown`
  articles, accessible through the `pkgdown` website of the package. (0.1.9.19)
- Updated the documentation of functions to state that they support
  `lavaan.mi`-class objects. (0.1.9.20)

# manymome 0.1.9

## Miscellaneous

- Modified some tests to accommodate a change in `lavaan`
  on handling random seed.
- Made some tests run faster to meet CRAN requirements.
- Used precomputed results to speed up the building of vignettes.

# manymome 0.1.6

## New Features

### Bootstrapping

- Added `lm2boot_out_parallel()` to do bootstrapping with
  the output of `lm()` using parallel processing. This is
  the default when `do_boot()` is used on the outputs of
  `lm()`. (0.1.4.4)
- Added a print method for the output of `do_boot()`. (0.1.4.7)

### Mediation

- Added `all_indirect_paths()` for identifying all indirect paths
  in a model. (0.1.4.5)
- Added `many_indirect_effects()` for computing indirect
  effects for a list of paths. (0.1.4.5)
- Added `total_indirect_effect()` for computing the total
  indirect effect between two variables. (0.1.4.5)

## Bug Fixes

- Updated README.md with code for installing from CRAN. (0.1.4.1).
- Fixed a typo in README.md. (0.1.4.2)
- Fixed tests that should be done by `expect_equal` on numbers rather
  than on characters. No change in the functions. (0.1.4.3)
- Noted in the vignettes that some new functions are not
  yet on the CRAN version and available in the GitHub
  version. (0.1.4.6)
- Fixed a bug in `merge_model_frame()`. (0.1.4.8)

## Others

- Finalize Version 0.1.6 for CRAN.

# manymome 0.1.4.0

- Checked examples and vignettes to ensure
  parallel processing is not used. (0.1.3.1)
- Updated the DESCRIPTION (0.1.3.2)
- Release version for main. (0.1.4.0)

# manymome 0.1.3.0

- Release version for main. (0.1.2.0)
- Fixed an invalid link in Readme.md. (0.1.3.0)

# manymome 0.1.1.6

- Cleaned up the doc and code.

# manymome 0.1.1.5

- First public release.
