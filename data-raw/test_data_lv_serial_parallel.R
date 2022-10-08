# Generate data
library(lavaan)
n <- 500
mod_pop <-
"
fx1 =~ .7*x1 + .7*x2 + .7*x3
fx2 =~ .7*x4 + .7*x5 + .7*x6
fm11 =~ .7*m11a + .7*m11b + .7*m11c
fm12 =~ .7*m12a + .7*m12b + .7*m12c
fm2 =~ .7*m2a + .7*m2b + .7*m2c
fy1 =~ .7*y1 + .7*y2 + .7*y3
fy2 =~ .7*y4 + .7*y5 + .7*y6
fm11 ~ .4 * fx1
fm12 ~ .3 * fm11
fm2 ~ .5 * fx2 + .2 * fx1
fy1 ~ .5 * fm12 + .3 * fx1
fy2 ~ .5 * fm2 + .3 * fx2
"
set.seed(80511)
dat <- simulateData(mod_pop, sample.nobs = n)
head(dat)
mod <-
"
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
"
fit <- sem(mod, dat)
fit
parameterEstimates(fit)
data_serial_parallel_latent <- dat
usethis::use_data(data_serial_parallel_latent, overwrite = TRUE)

