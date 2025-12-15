# Print the Output of 'cond_indirect_diff'

Print the output of
[`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md).

## Usage

``` r
# S3 method for class 'cond_indirect_diff'
print(x, digits = 3, pvalue = FALSE, pvalue_digits = 3, se = FALSE, ...)
```

## Arguments

- x:

  The output of
  [`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md).

- digits:

  The number of decimal places in the printout.

- pvalue:

  Logical. If `TRUE`, asymmetric *p*-value based on bootstrapping will
  be printed if available. Default is `FALSE.`

- pvalue_digits:

  Number of decimal places to display for the *p*-value. Default is 3.

- se:

  Logical. If `TRUE` and confidence intervals are available, the
  standard errors of the estimates are also printed. They are simply the
  standard deviations of the bootstrap estimates or Monte Carlo
  simulated values, depending on the method used to form the confidence
  intervals.

- ...:

  Optional arguments. Ignored.

## Value

It returns `x` invisibly. Called for its side effect.

## Details

The `print` method of the `cond_indirect_diff`-class object.

If bootstrapping confidence interval was requested, this method has the
option to print a *p*-value computed by the method presented in
Asparouhov and Muthén (2021). Note that this *p*-value is asymmetric
bootstrap *p*-value based on the distribution of the bootstrap
estimates. It is not computed based on the distribution under the null
hypothesis.

For a *p*-value of *a*, it means that a 100(1 - *a*)% bootstrapping
confidence interval will have one of its limits equal to 0. A confidence
interval with a higher confidence level will include zero, while a
confidence interval with a lower confidence level will exclude zero.

## References

Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
Retrieved from
https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf

## See also

[`cond_indirect_diff()`](https://sfcheung.github.io/manymome/reference/cond_indirect_diff.md)
