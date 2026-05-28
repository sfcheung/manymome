# Sample Dataset: With Reverse Items

Generated from a complicated mediation model among several latent
factors, `fx`, `fm1`, `fm2`, `fm3`, and `fy`, along with two control
variables, `c1` and `c2`, each has four indicators, and some are reverse
items.

## Usage

``` r
data_indicators
```

## Format

A data frame with 500 rows and 11 variables:

- x_1:

  Indicator of `fx`. Numeric.

- x_2:

  Indicator of `fx`. Numeric.

- x_3:

  Indicator of `fx`. Numeric.

- x_4:

  Indicator of `fx`. Numeric. A reverse item.

- m1_1:

  Indicator of `fm1`. Numeric.

- m1_2:

  Indicator of `fm1`. Numeric.

- m1_3:

  Indicator of `fm1`. Numeric.

- m1_4:

  Indicator of `fm1`. Numeric.

- m2_1:

  Indicator of `fm2`. Numeric.

- m2_2:

  Indicator of `fm2`. Numeric.

- m2_3:

  Indicator of `fm2`. Numeric.

- m2_4:

  Indicator of `fm2`. Numeric.

- m3_1:

  Indicator of `fm3`. Numeric.

- m3_2:

  Indicator of `fm3`. Numeric.

- m3_3:

  Indicator of `fm3`. Numeric.

- m3_4:

  Indicator of `fm3`. Numeric.

- y_1:

  Indicator of `fy`. Numeric.

- y_2:

  Indicator of `fy`. Numeric.

- y_3:

  Indicator of `fy`. Numeric.

- y_4:

  Indicator of `fy`. Numeric. A reverse item.

- c1_1:

  Indicator of `fc1`. Numeric.

- c1_2:

  Indicator of `fc1`. Numeric.

- c1_3:

  Indicator of `fc1`. Numeric.

- c1_4:

  Indicator of `fc1`. Numeric.

- c2_1:

  Indicator of `fc2`. Numeric.

- c2_2:

  Indicator of `fc2`. Numeric.

- c2_3:

  Indicator of `fc2`. Numeric.

- c2_4:

  Indicator of `fc2`. Numeric.

- x:

  Mean of the indicators of `x`. Numeric.

- m1:

  Mean of the indicators of `m1`. Numeric.

- m2:

  Mean of the indicators of `m2`. Numeric.

- m3:

  Mean of the indicators of `m3`. Numeric.

- y:

  Mean of the indicators of `y`. Numeric.

- c1:

  Mean of the indicators of `c1`. Numeric.

- c2:

  Mean of the indicators of `c2`. Numeric.

## Details

The model:

    fx =~ x_1 + x_2 + x_3 + x_4
    fm1 =~ m1_1 + m1_2 + m1_3 + m1_4
    fm2 =~ m2_1 + m2_2 + m2_3 + m2_4
    fm3 =~ m3_1 + m3_2 + m3_3 + m3_4
    fy =~ y_1 + y_2 + y_3 + y_4
    fc1 =~ c1_1 + c1_2 + c1_3 + c1_4
    fc2 =~ c2_1 + c2_2 + c2_3 + c2_4
    fm1 ~ a1 * fx + fc1 + fc2
    fm3 ~ b1 * fm1 + fc1 + fc2
    fm2 ~ a2 * fx + fc1 + fc2
    fy ~ b3 * fm3 + b2 * fm2 + fc1 + fc2
    ab13 := a1 * b1 * b3
    ab2 := a2 * b2
