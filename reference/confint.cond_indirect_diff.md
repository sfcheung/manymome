# Confidence Interval of the Output of 'cond_indirect_diff()'

Extract the confidence interval the output of
[`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md).

## Usage

``` r
# S3 method for class 'cond_indirect_diff'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  The output of
  [`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md).

- parm:

  Ignored.

- level:

  The level of confidence for the confidence interval. Default is .95.
  Must match the level of the stored confidence interval.

- ...:

  Optional arguments. Ignored.

## Value

A one-row-two-column data frame of the confidence limits. If confidence
interval is not available, the limits are `NA`s.

## Details

The `confint` method of the `cond_indirect_diff`-class object.

The type of confidence intervals depends on the call used to create the
object. This function merely extracts the stored confidence intervals.
