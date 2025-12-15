# Sample Dataset: A Latent Mediation Model With Three Mediators

Generated from a 3-mediator mediation model among eight latent factors,
`fx1`, `fx2`, `fm11`, `fm12`, `fy1`, and `fy2`, each has three
indicators.

## Usage

``` r
data_serial_parallel_latent
```

## Format

A data frame with 500 rows and 21 variables:

- x1:

  Indicator of `fx1`. Numeric.

- x2:

  Indicator of `fx1`. Numeric.

- x3:

  Indicator of `fx1`. Numeric.

- x4:

  Indicator of `fx2`. Numeric.

- x5:

  Indicator of `fx2`. Numeric.

- x6:

  Indicator of `fx2`. Numeric.

- m11a:

  Indicator of `fm11`. Numeric.

- m11b:

  Indicator of `fm11`. Numeric.

- m11c:

  Indicator of `fm11`. Numeric.

- m12a:

  Indicator of `fm12`. Numeric.

- m12b:

  Indicator of `fm12`. Numeric.

- m12c:

  Indicator of `fm12`. Numeric.

- m2a:

  Indicator of `fm2`. Numeric.

- m2b:

  Indicator of `fm2`. Numeric.

- m2c:

  Indicator of `fm2`. Numeric.

- y1:

  Indicator of `fy1`. Numeric.

- y2:

  Indicator of `fy1`. Numeric.

- y3:

  Indicator of `fy1`. Numeric.

- y4:

  Indicator of `fy2`. Numeric.

- y5:

  Indicator of `fy2`. Numeric.

- y6:

  Indicator of `fy2`. Numeric.

## Details

The model:

    fx1 =~ x1 + x2 + x3
    fx2 =~ x4 + x5 + x6
    fm11 =~ m11a + m11b + m11c
    fm12 =~ m12a + m12b + m12c
    fm2  =~ m2a + m2b + m2c
    fy1 =~ y1 + y2 + y3
    fy2 =~ y3 + y4 + y5
    fm11 ~ a1 * fx1
    fm12 ~ b11 * fm11 + a2m * fx2
    fm2 ~ a2 * fx2
    fy1 ~ b12 * fm12 + b11y1 * fm11 + cp1 * fx1
    fy2 ~ b2 * fm2 + cp2 * fx2
    a1b11b12 := a1 * b11 * b12
    a1b11y1 := a1 * b11y1
    a2b2 := a2 * b2
    a2mb12 := a2m * b12
