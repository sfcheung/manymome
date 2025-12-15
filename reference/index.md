# Package index

## Main Functions

Compute conditional indirect effects, conditional effects, and indirect
effects.

- [`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  [`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  [`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  [`cond_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  [`many_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
  : Conditional, Indirect, and Conditional Indirect Effects
- [`all_indirect_paths()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)
  [`all_paths_to_df()`](https://sfcheung.github.io/manymome/reference/all_indirect_paths.md)
  : Enumerate All Indirect Effects in a Model
- [`indirect_effects_from_list()`](https://sfcheung.github.io/manymome/reference/indirect_effects_from_list.md)
  : Coefficient Table of an 'indirect_list' Class Object
- [`total_indirect_effect()`](https://sfcheung.github.io/manymome/reference/total_indirect_effect.md)
  : Total Indirect Effect Between Two Variables

## Quick Functions for Common Models

- [`q_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md)
  [`q_simple_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md)
  [`q_serial_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md)
  [`q_parallel_mediation()`](https://sfcheung.github.io/manymome/reference/q_mediation.md)
  [`print(`*`<q_mediation>`*`)`](https://sfcheung.github.io/manymome/reference/q_mediation.md)
  : Mediation Models By Regression or SEM
- [`plot(`*`<q_mediation>`*`)`](https://sfcheung.github.io/manymome/reference/plot.q_mediation.md)
  [`indirect_on_plot()`](https://sfcheung.github.io/manymome/reference/plot.q_mediation.md)
  : Plot Method for the Output of 'q_mediation' Family

## Presenting and Exploring the Effects

- [`index_of_mome()`](https://sfcheung.github.io/manymome/reference/index_of_mome.md)
  [`index_of_momome()`](https://sfcheung.github.io/manymome/reference/index_of_mome.md)
  : Index of Moderated Mediation and Index of Moderated Moderated
  Mediation
- [`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md)
  : Differences In Conditional Indirect Effects
- [`plot(`*`<cond_indirect_effects>`*`)`](https://sfcheung.github.io/manymome/reference/plot.cond_indirect_effects.md)
  : Plot Conditional Effects
- [`plot_effect_vs_w()`](https://sfcheung.github.io/manymome/reference/plot_effect_vs_w.md)
  [`fill_wlevels()`](https://sfcheung.github.io/manymome/reference/plot_effect_vs_w.md)
  : Plot an Effect Against a Moderator
- [`pseudo_johnson_neyman()`](https://sfcheung.github.io/manymome/reference/pseudo_johnson_neyman.md)
  [`johnson_neyman()`](https://sfcheung.github.io/manymome/reference/pseudo_johnson_neyman.md)
  [`print(`*`<pseudo_johnson_neyman>`*`)`](https://sfcheung.github.io/manymome/reference/pseudo_johnson_neyman.md)
  : Pseudo Johnson-Neyman Probing
- [`indirect_proportion()`](https://sfcheung.github.io/manymome/reference/indirect_proportion.md)
  : Proportion of Effect Mediated
- [`delta_med()`](https://sfcheung.github.io/manymome/reference/delta_med.md)
  : Delta_Med by Liu, Yuan, and Li (2023)

## Moderator Levels

Set the levels of moderators to be used in
[`cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

- [`mod_levels()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  [`mod_levels_list()`](https://sfcheung.github.io/manymome/reference/mod_levels.md)
  : Create Levels of Moderators
- [`merge_mod_levels()`](https://sfcheung.github.io/manymome/reference/merge_mod_levels.md)
  : Merge the Generated Levels of Moderators

## Bootstrapping

Generate bootstrap estimates to be used by the main functions to form
bootstrap confidence intervals

- [`do_boot()`](https://sfcheung.github.io/manymome/reference/do_boot.md)
  : Bootstrap Estimates for 'indirect_effects' and
  'cond_indirect_effects'

- [`lm2boot_out()`](https://sfcheung.github.io/manymome/reference/lm2boot_out.md)
  [`lm2boot_out_parallel()`](https://sfcheung.github.io/manymome/reference/lm2boot_out.md)
  :

  Bootstrap Estimates for `lm` Outputs

- [`fit2boot_out()`](https://sfcheung.github.io/manymome/reference/fit2boot_out.md)
  [`fit2boot_out_do_boot()`](https://sfcheung.github.io/manymome/reference/fit2boot_out.md)
  :

  Bootstrap Estimates for a `lavaan` Output

## Monte Carlo

Generate simulated estimates to be used by the main functions to form
Monte Carlo confidence intervals

- [`do_mc()`](https://sfcheung.github.io/manymome/reference/do_mc.md)
  [`gen_mc_est()`](https://sfcheung.github.io/manymome/reference/do_mc.md)
  : Monte Carlo Estimates for 'indirect_effects' and
  'cond_indirect_effects'

- [`fit2mc_out()`](https://sfcheung.github.io/manymome/reference/fit2mc_out.md)
  :

  Monte Carlo Estimates for a `lavaan` Output

## For ‘lavaan’

- [`factor2var()`](https://sfcheung.github.io/manymome/reference/factor2var.md)
  : Create Dummy Variables

## For ‘lm’

- [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md)
  : Join 'lm()' Output to Form an 'lm_list\`-Class Object

## Methods

Methods and utility functions for the output of the main functions.

- [`coef(`*`<indirect>`*`)`](https://sfcheung.github.io/manymome/reference/coef.indirect.md)
  : Extract the Indirect Effect or Conditional Indirect Effect

- [`confint(`*`<indirect>`*`)`](https://sfcheung.github.io/manymome/reference/confint.indirect.md)
  : Confidence Interval of Indirect Effect or Conditional Indirect
  Effect

- [`print(`*`<indirect>`*`)`](https://sfcheung.github.io/manymome/reference/print.indirect.md)
  : Print an 'indirect' Class Object

- [`` `+`( ``*`<indirect>`*`)`](https://sfcheung.github.io/manymome/reference/math_indirect.md)
  [`` `-`( ``*`<indirect>`*`)`](https://sfcheung.github.io/manymome/reference/math_indirect.md)
  : Math Operators for 'indirect'-Class Objects

- [`coef(`*`<indirect_list>`*`)`](https://sfcheung.github.io/manymome/reference/coef.indirect_list.md)
  : Extract the Indirect Effects from a 'indirect_list' Object

- [`confint(`*`<indirect_list>`*`)`](https://sfcheung.github.io/manymome/reference/confint.indirect_list.md)
  : Confidence Intervals of Indirect Effects in an 'indirect_list'
  Object

- [`print(`*`<indirect_list>`*`)`](https://sfcheung.github.io/manymome/reference/print.indirect_list.md)
  : Print an 'indirect_list' Class Object

- [`coef(`*`<cond_indirect_effects>`*`)`](https://sfcheung.github.io/manymome/reference/coef.cond_indirect_effects.md)
  : Estimates of Conditional Indirect Effects or Conditional Effects

- [`confint(`*`<cond_indirect_effects>`*`)`](https://sfcheung.github.io/manymome/reference/confint.cond_indirect_effects.md)
  : Confidence Intervals of Indirect Effects or Conditional Indirect
  Effects

- [`print(`*`<cond_indirect_effects>`*`)`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_effects.md)
  [`as.data.frame(`*`<cond_indirect_effects>`*`)`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_effects.md)
  : Print a 'cond_indirect_effects' Class Object

- [`` `[`( ``*`<cond_indirect_effects>`*`)`](https://sfcheung.github.io/manymome/reference/subsetting_cond_indirect_effects.md)
  : Extraction Methods for 'cond_indirect_effects' Outputs

- [`get_one_cond_indirect_effect()`](https://sfcheung.github.io/manymome/reference/get_one_cond_indirect_effect.md)
  [`get_one_cond_effect()`](https://sfcheung.github.io/manymome/reference/get_one_cond_indirect_effect.md)
  [`print_all_cond_indirect_effects()`](https://sfcheung.github.io/manymome/reference/get_one_cond_indirect_effect.md)
  [`print_all_cond_effects()`](https://sfcheung.github.io/manymome/reference/get_one_cond_indirect_effect.md)
  : Get The Conditional Indirect Effect for One Row of
  'cond_indirect_effects' Output

- [`coef(`*`<cond_indirect_diff>`*`)`](https://sfcheung.github.io/manymome/reference/coef.cond_indirect_diff.md)
  : Print the Output of 'cond_indirect_diff()'

- [`confint(`*`<cond_indirect_diff>`*`)`](https://sfcheung.github.io/manymome/reference/confint.cond_indirect_diff.md)
  : Confidence Interval of the Output of 'cond_indirect_diff()'

- [`print(`*`<cond_indirect_diff>`*`)`](https://sfcheung.github.io/manymome/reference/print.cond_indirect_diff.md)
  : Print the Output of 'cond_indirect_diff'

- [`print(`*`<lm_list>`*`)`](https://sfcheung.github.io/manymome/reference/print.lm_list.md)
  :

  Print an `lm_list`-Class Object

- [`summary(`*`<lm_list>`*`)`](https://sfcheung.github.io/manymome/reference/summary.lm_list.md)
  [`print(`*`<summary_lm_list>`*`)`](https://sfcheung.github.io/manymome/reference/summary.lm_list.md)
  :

  Summary of an `lm_list`-Class Object

- [`` `[`( ``*`<wlevels>`*`)`](https://sfcheung.github.io/manymome/reference/subsetting_wlevels.md)
  [`` `[<-`( ``*`<wlevels>`*`)`](https://sfcheung.github.io/manymome/reference/subsetting_wlevels.md)
  [`` `[[<-`( ``*`<wlevels>`*`)`](https://sfcheung.github.io/manymome/reference/subsetting_wlevels.md)
  : Extraction Methods for a 'wlevels'-class Object

- [`print(`*`<all_paths>`*`)`](https://sfcheung.github.io/manymome/reference/print.all_paths.md)
  : Print 'all_paths' Class Object

- [`print(`*`<boot_out>`*`)`](https://sfcheung.github.io/manymome/reference/print.boot_out.md)
  :

  Print a `boot_out`-Class Object

- [`print(`*`<mc_out>`*`)`](https://sfcheung.github.io/manymome/reference/print.mc_out.md)
  :

  Print a `mc_out`-Class Object

- [`print(`*`<indirect_proportion>`*`)`](https://sfcheung.github.io/manymome/reference/print.indirect_proportion.md)
  : Print an 'indirect_proportion'-Class Object

- [`coef(`*`<indirect_proportion>`*`)`](https://sfcheung.github.io/manymome/reference/coef.indirect_proportion.md)
  : Extract the Proportion of Effect Mediated

- [`print(`*`<delta_med>`*`)`](https://sfcheung.github.io/manymome/reference/print.delta_med.md)
  : Print a 'delta_med' Class Object

- [`coef(`*`<delta_med>`*`)`](https://sfcheung.github.io/manymome/reference/coef.delta_med.md)
  : Delta_Med in a 'delta_med'-Class Object

- [`confint(`*`<delta_med>`*`)`](https://sfcheung.github.io/manymome/reference/confint.delta_med.md)
  : Confidence Interval for Delta_Med in a 'delta_med'-Class Object

## Advanced helpers

Helper functions exported for advanced users.

- [`indirect_i()`](https://sfcheung.github.io/manymome/reference/indirect_i.md)
  : Indirect Effect (No Bootstrapping)
- [`check_path()`](https://sfcheung.github.io/manymome/reference/check_path.md)
  : Check a Path Exists in a Model
- [`lm_from_lavaan_list()`](https://sfcheung.github.io/manymome/reference/lm_from_lavaan_list.md)
  : 'lavaan'-class to 'lm_from_lavaan_list'-Class
- [`coef(`*`<lm_from_lavaan>`*`)`](https://sfcheung.github.io/manymome/reference/coef.lm_from_lavaan.md)
  : Coefficients of an 'lm_from_lavaan'-Class Object
- [`terms(`*`<lm_from_lavaan>`*`)`](https://sfcheung.github.io/manymome/reference/terms.lm_from_lavaan.md)
  : Model Terms of an 'lm_from_lavaan'-Class Object
- [`predict(`*`<lm_from_lavaan>`*`)`](https://sfcheung.github.io/manymome/reference/predict.lm_from_lavaan.md)
  : Predicted Values of a 'lm_from_lavaan'-Class Object
- [`predict(`*`<lm_from_lavaan_list>`*`)`](https://sfcheung.github.io/manymome/reference/predict.lm_from_lavaan_list.md)
  : Predicted Values of an 'lm_from_lavaan_list'-Class Object
- [`predict(`*`<lm_list>`*`)`](https://sfcheung.github.io/manymome/reference/predict.lm_list.md)
  : Predicted Values of an 'lm_list'-Class Object
- [`get_prod()`](https://sfcheung.github.io/manymome/reference/get_prod.md)
  : Product Terms (if Any) Along a Path

## Datasets

Datasets used in examples.

- [`data_med`](https://sfcheung.github.io/manymome/reference/data_med.md)
  : Sample Dataset: Simple Mediation
- [`data_med_complicated`](https://sfcheung.github.io/manymome/reference/data_med_complicated.md)
  : Sample Dataset: A Complicated Mediation Model
- [`data_med_complicated_mg`](https://sfcheung.github.io/manymome/reference/data_med_complicated_mg.md)
  : Sample Dataset: A Complicated Mediation Model With Two Groups
- [`data_med_mg`](https://sfcheung.github.io/manymome/reference/data_med_mg.md)
  : Sample Dataset: Simple Mediation With Two Groups
- [`data_med_mod_a`](https://sfcheung.github.io/manymome/reference/data_med_mod_a.md)
  : Sample Dataset: Simple Mediation with a-Path Moderated
- [`data_med_mod_ab`](https://sfcheung.github.io/manymome/reference/data_med_mod_ab.md)
  : Sample Dataset: Simple Mediation with Both Paths Moderated (Two
  Moderators)
- [`data_med_mod_ab1`](https://sfcheung.github.io/manymome/reference/data_med_mod_ab1.md)
  : Sample Dataset: Simple Mediation with Both Paths Moderated By a
  Moderator
- [`data_med_mod_b`](https://sfcheung.github.io/manymome/reference/data_med_mod_b.md)
  : Sample Dataset: Simple Mediation with b-Path Moderated
- [`data_med_mod_b_mod`](https://sfcheung.github.io/manymome/reference/data_med_mod_b_mod.md)
  : Sample Dataset: A Simple Mediation Model with b-Path
  Moderated-Moderation
- [`data_med_mod_parallel`](https://sfcheung.github.io/manymome/reference/data_med_mod_parallel.md)
  : Sample Dataset: Parallel Mediation with Two Moderators
- [`data_med_mod_parallel_cat`](https://sfcheung.github.io/manymome/reference/data_med_mod_parallel_cat.md)
  : Sample Dataset: Parallel Moderated Mediation with Two Categorical
  Moderators
- [`data_med_mod_serial`](https://sfcheung.github.io/manymome/reference/data_med_mod_serial.md)
  : Sample Dataset: Serial Mediation with Two Moderators
- [`data_med_mod_serial_cat`](https://sfcheung.github.io/manymome/reference/data_med_mod_serial_cat.md)
  : Sample Dataset: Serial Moderated Mediation with Two Categorical
  Moderators
- [`data_med_mod_serial_parallel`](https://sfcheung.github.io/manymome/reference/data_med_mod_serial_parallel.md)
  : Sample Dataset: Serial-Parallel Mediation with Two Moderators
- [`data_med_mod_serial_parallel_cat`](https://sfcheung.github.io/manymome/reference/data_med_mod_serial_parallel_cat.md)
  : Sample Dataset: Serial-Parallel Moderated Mediation with Two
  Categorical Moderators
- [`data_mod`](https://sfcheung.github.io/manymome/reference/data_mod.md)
  : Sample Dataset: One Moderator
- [`data_mod2`](https://sfcheung.github.io/manymome/reference/data_mod2.md)
  : Sample Dataset: Two Moderators
- [`data_mod_cat`](https://sfcheung.github.io/manymome/reference/data_mod_cat.md)
  : Sample Dataset: Moderation with One Categorical Moderator
- [`data_mome_demo`](https://sfcheung.github.io/manymome/reference/data_mome_demo.md)
  : Sample Dataset: A Complicated Moderated-Mediation Model
- [`data_mome_demo_missing`](https://sfcheung.github.io/manymome/reference/data_mome_demo_missing.md)
  : Sample Dataset: A Complicated Moderated-Mediation Model With Missing
  Data
- [`data_parallel`](https://sfcheung.github.io/manymome/reference/data_parallel.md)
  : Sample Dataset: Parallel Mediation
- [`data_sem`](https://sfcheung.github.io/manymome/reference/data_sem.md)
  : Sample Dataset: A Latent Variable Mediation Model With 4 Factors
- [`data_serial`](https://sfcheung.github.io/manymome/reference/data_serial.md)
  : Sample Dataset: Serial Mediation
- [`data_serial_parallel`](https://sfcheung.github.io/manymome/reference/data_serial_parallel.md)
  : Sample Dataset: Serial-Parallel Mediation
- [`data_serial_parallel_latent`](https://sfcheung.github.io/manymome/reference/data_serial_parallel_latent.md)
  : Sample Dataset: A Latent Mediation Model With Three Mediators
- [`modmed_x1m3w4y1`](https://sfcheung.github.io/manymome/reference/modmed_x1m3w4y1.md)
  : Sample Dataset: Moderated Serial Mediation
- [`simple_mediation_latent`](https://sfcheung.github.io/manymome/reference/simple_mediation_latent.md)
  : Sample Dataset: A Simple Latent Mediation Model
