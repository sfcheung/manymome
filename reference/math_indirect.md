# Math Operators for 'indirect'-Class Objects

Mathematic operators for 'indirect'-class object, the output of
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md).

## Usage

``` r
# S3 method for class 'indirect'
e1 + e2

# S3 method for class 'indirect'
e1 - e2
```

## Arguments

- e1:

  An 'indirect'-class object.

- e2:

  An 'indirect'-class object.

## Value

An 'indirect'-class object with a list of effects stored. See
[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
on details for this class.

## Details

For now, only `+` operator and `-` operator are supported. These
operators can be used to estimate and test a function of effects between
the same pair of variables.

For example, they can be used to compute and test the total effects
along different paths. They can also be used to compute and test the
difference between the effects along two paths.

The operators will check whether an operation is valid. An operation is
not valid if

1.  the two paths do not start from the same variable,

2.  the two paths do not end at the same variable,

3.  moderators are involved but they are not set to the same values in
    both objects, and

4.  bootstrap estimates stored in `boot_out`, if any, are not identical.

5.  Monte Carlo simulated estimates stored in `mc_out`, if any, are not
    identical.

If bootstrap estimates are stored and both objects used the same type of
bootstrap confidence interval, that type will be used. Otherwise,
percentile bootstrap confidence interval, the recommended method, will
be used.

### Multigroup Models

Since Version 0.1.14.2, support for multigroup models has been added for
models fitted by `lavaan`. Both bootstrapping and Monte Carlo confidence
intervals are supported. These operators can be used to compute and test
the difference of an indirect effect between two groups. This can also
be used to compute and test the difference between a function of effects
between groups, for example, the total indirect effects between two
groups.

The operators are flexible and allow users to do many possible
computations. Therefore, users need to make sure that the function of
effects is meaningful.

## See also

[`indirect_effect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)
and
[`cond_indirect()`](https://sfcheung.github.io/manymome/reference/cond_indirect.md)

## Examples

``` r
library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x  + d1 * w1 + e1 * x:w1
m2 ~ m1 + a2 * x
y  ~ b1 * m1 + b2 * m2 + cp * x
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)
hi_w1 <- mean(dat$w1) + sd(dat$w1)

# Examples for cond_indirect():

# Conditional effect from x to m1 when w1 is 1 SD above mean
out1 <- cond_indirect(x = "x", y = "y", m = c("m1", "m2"),
              wvalues = c(w1 = hi_w1), fit = fit)
out2 <- cond_indirect(x = "x", y = "y", m = c("m2"),
              wvalues = c(w1 = hi_w1), fit = fit)
out3 <- cond_indirect(x = "x", y = "y",
              wvalues = c(w1 = hi_w1), fit = fit)

out12 <- out1 + out2
out12
#> 
#> == Conditional Indirect Effect   ==
#>                                         
#>  Path:                x -> m1 -> m2 -> y
#>  Path:                x -> m2 -> y      
#>  Moderators:          w1                
#>  Function of Effects: 0.082             
#>  When:                w1 = 1.228        
#> 
#> Computation of the Function of Effects:
#>  (x->m1->m2->y)
#> +(x->m2->y) 
#> 
out123 <- out1 + out2 + out3
out123
#> 
#> == Conditional Indirect Effect   ==
#>                                         
#>  Path:                x -> m1 -> m2 -> y
#>  Path:                x -> m2 -> y      
#>  Path:                x -> y            
#>  Moderators:          w1                
#>  Function of Effects: 0.394             
#>  When:                w1 = 1.228        
#> 
#> Computation of the Function of Effects:
#>  ((x->m1->m2->y)
#> +(x->m2->y))
#> +(x->y) 
#> 
coef(out1) + coef(out2) + coef(out3)
#>     y~x 
#> 0.39405 

# Multigroup model with indirect effects

dat <- data_med_mg
mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE,
           group = "group")

# If a model has more than one group,
# the argument 'group' must be set.
ind1 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = fit,
                        group = "Group A")
ind1
#> 
#> == Indirect Effect  ==
#>                                          
#>  Path:            Group A[1]: x -> m -> y
#>  Indirect Effect: 0.409                  
#> 
#> Computation Formula:
#>   (b.m~x)*(b.y~m)
#> 
#> Computation:
#>   (0.87989)*(0.46481)
#> 
#> Coefficients of Component Paths:
#>  Path Coefficient
#>   m~x       0.880
#>   y~m       0.465
#> 
#> NOTE:
#> - The group label is printed before each path.
#> - The group number in square brackets is the number used internally in
#>   lavaan.
#> 
ind2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = fit,
                        group = 2)
ind2
#> 
#> == Indirect Effect  ==
#>                                          
#>  Path:            Group B[2]: x -> m -> y
#>  Indirect Effect: 0.663                  
#> 
#> Computation Formula:
#>   (b.m~x)*(b.y~m)
#> 
#> Computation:
#>   (0.59716)*(1.11040)
#> 
#> Coefficients of Component Paths:
#>  Path Coefficient
#>   m~x       0.597
#>   y~m       1.110
#> 
#> NOTE:
#> - The group label is printed before each path.
#> - The group number in square brackets is the number used internally in
#>   lavaan.
#> 

# Compute the difference in indirect effects between groups
ind2 - ind1
#> 
#> == Indirect Effect  ==
#>                                              
#>  Path:                Group B[2]: x -> m -> y
#>  Path:                Group A[1]: x -> m -> y
#>  Function of Effects: 0.254                  
#> 
#> Computation of the Function of Effects:
#>  (Group B[2]: x->m->y)
#> -(Group A[1]: x->m->y) 
#> 
#> NOTE:
#> - The group label is printed before each path.
#> - The group number in square brackets is the number used internally in
#>   lavaan.
#> 
```
