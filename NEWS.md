# manymome 0.1.9.7

- Updated badges in README.md. (0.1.9.1)
- Updated pkgdown site. (0.1.9.2)
- Added support for Monte Carlo confidence intervals. (0.1.9.3 to 0.1.9.4)
- Updated some vignettes for Monte Carlo confidence intervals. (0.1.9.5)
- Used a more reliable test for Monte Carlo CIs. (0.1.9.6)
- Fixed an error in pkgdown site. (0.1.9.6)
- Updated the logo for readability. (0.1.9.6)
- Some print methods support printing
  asymmetric bootstrap
  *p*-values using the method presented
  in Asparouhov and Muthén (2021) if bootstrapping
  confidence interval is requested. By
  default, *p*-values are not printed. (0.1.9.7)

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
