# Print an `lm_list`-Class Object

Print the content of the output of
[`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

## Usage

``` r
# S3 method for class 'lm_list'
print(x, ...)
```

## Arguments

- x:

  The output of
  [`lm2list()`](https://sfcheung.github.io/manymome/reference/lm2list.md).

- ...:

  Other arguments. Not used.

## Value

`x` is returned invisibly. Called for its side effect.

## Examples

``` r
data(data_serial_parallel)
lm_m11 <- lm(m11 ~ x + c1 + c2, data_serial_parallel)
lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, data_serial_parallel)
lm_m2 <- lm(m2 ~ x + c1 + c2, data_serial_parallel)
lm_y <- lm(y ~ m11 + m12 + m2 + x + c1 + c2, data_serial_parallel)
# Join them to form a lm_list-class object
lm_serial_parallel <- lm2list(lm_m11, lm_m12, lm_m2, lm_y)
lm_serial_parallel
#> 
#> The model(s):
#> m11 ~ x + c1 + c2
#> m12 ~ m11 + x + c1 + c2
#> m2 ~ x + c1 + c2
#> y ~ m11 + m12 + m2 + x + c1 + c2
#> 
```
