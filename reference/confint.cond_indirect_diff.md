# Confidence Interval of the Output of 'cond_indirect_diff()'

Extract the confidence interval of the output of
[`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md).

## Usage

``` r
# S3 method for class 'cond_indirect_diff'
confint(object, parm, level = NULL, ...)
```

## Arguments

- object:

  The output of
  [`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md).

- parm:

  Ignored.

- level:

  If set to `NULL`, the default, then the level of confidence used to
  generate `object` is used. If set to a value, this value will be used
  to recompute the confidence intervals. If the confidence interval is
  to be computed from the standard error, and so `level` is not set in
  `object`, then the default value is .95. (This new behavior applies to
  0.3.6.15 and later versions.)

- ...:

  Optional arguments. Ignored.

## Value

A one-row-two-column data frame of the confidence limits. If confidence
interval is not available, the limits are `NA`s.

## Details

The `confint` method of the `cond_indirect_diff`-class object.

The type of confidence intervals depends on the call used to create the
object. This function merely extracts the stored confidence intervals.
