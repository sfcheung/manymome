# Generate data
library(lavaan)
n <- 200
mod_pop <-
"
fx =~ .7*x1 + .7*x2 + .7*x3 + .3 * m1
fm =~ .7*m1 + .7*m2 + .7*m3 + .4 * y2
fy =~ .7*y1 + .7*y2 + .7*y3 + .2 * x1
fm ~ .4 * fx
fy ~ .5 * fm + .2 * fx
"
set.seed(80511)
dat <- simulateData(mod_pop, sample.nobs = n)
head(dat)
mod <-
"
fx =~ x1 + x2 + x3 + m1
fm =~ m1 + m2 + m3 + y2
fy =~ y1 + y2 + y3 + x1
fm ~ a*fx
fy ~ b*fm + cp*fx
indirect := a*b
"
fit <- sem(mod, dat)
fit
parameterEstimates(fit)
simple_mediation_latent <- dat
usethis::use_data(simple_mediation_latent, overwrite = TRUE)

