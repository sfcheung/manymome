# Sample Dataset: A Complicated Moderated-Mediation Model With Missing Data

Generated from a complicated moderated-mediation model for
demonstration, with missing data

## Usage

``` r
data_mome_demo_missing
```

## Format

A data frame with 200 rows and 11 variables:

- x1:

  Predictor 1. Numeric.

- x2:

  Predictor 2. Numeric.

- m1:

  Mediator 1. Numeric.

- m2:

  Mediator 2. Numeric.

- m3:

  Mediator 3. Numeric.

- y1:

  Outcome Variable 1. Numeric.

- y2:

  Outcome Variable 2. Numeric.

- w1:

  Moderator 1. Numeric.

- w2:

  Moderator 21. Numeric.

- c1:

  Control Variable 1. Numeric.

- c2:

  Control Variable 2. Numeric.

## Details

A copy of
[data_mome_demo](https://sfcheung.github.io/manymome/reference/data_mome_demo.md)
with some randomly selected cells changed to `NA`. The number of cases
with no missing data is 169.

The model:

    # w1x1 <- x1 * w1
    # w2m2 <- w2 * m2
    m1 ~ x1 + w1 + w1x1 + x2 + c1 + c2
    m2 ~ m1 + c1 + c2
    m3 ~ x2 + x1 + c1 + c2
    y1 ~ m2 + w2 + w2m2 + x1 + x2 + m3 + c1 + c2
    y2 ~ m3 + x2 + x1 + m2 + c1 + c2
    # Covariances excluded for brevity
