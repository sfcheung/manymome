# Sample Dataset: A Simple Latent Mediation Model

Generated from a simple mediation model among xthree latent factors,
`fx`, `fm`, and `fy`, xeach has three indicators.

## Usage

``` r
simple_mediation_latent
```

## Format

A data frame with 200 rows and 11 variables:

- x1:

  Indicator of `fx`. Numeric.

- x2:

  Indicator of `fx`. Numeric.

- x3:

  Indicator of `fx`. Numeric.

- m1:

  Indicator of `fm`. Numeric.

- m2:

  Indicator of `fm`. Numeric.

- m3:

  Indicator of `fm`. Numeric.

- y1:

  Indicator of `fy`. Numeric.

- y2:

  Indicator of `fy`. Numeric.

- y3:

  Indicator of `fy`. Numeric.

## Details

The model:

    fx =~ x1 + x2 + x3
    fm =~ m1 + m2 + m3
    fy =~ y1 + y2 + y3
    fm ~ a * fx
    fy ~ b * fm + cp * fx
    indirect := a * b
