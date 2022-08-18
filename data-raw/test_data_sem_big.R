#' #Create several datasets for testing purpose.
#'
#' A latent variable model
library(lavaan)
modp <-
  'f1 =~ 1.0*x01 + 0.8*x02 + 1.2*x03
   f2 =~ 1.0*x04 + 0.0*x05 + 1.2*x06 + 0.8*x07
   f3 =~ 1.0*x08 + 0.8*x09 + 1.2*x10
   f4 =~ 1.0*x11 + 0.8*x12 + 0.0*x13 + 1.1*x14
   f3 ~  0.27*f1 + 0.136*f2
   f4 ~  0.35*f1 + 0.3*f2 + 0.4*f3
  '
mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  a1*f1 + a2*f2
   f4 ~  b1*f1 + b3*f3
   a1b3 := a1 * b3
   a2b3 := a2 * b3
  '

# generate data
set.seed(246426)
dat <- lavaan::simulateData(modp, sample.nobs = 200L)
fit <- lavaan::sem(mod, dat)
est <- parameterEstimates(fit)
est[est$op %in% c("~", ":="), ]
std <- standardizedSolution(fit)
std[std$op %in% c("~", ":="), ]

# fit_boot <- do_boot(fit, R = 2000, seed = 897543)
# a1b3 <- indirect_effect(x = "f1", y = "f4", m = "f3", fit = fit,
#                         boot_ci = TRUE, boot_out = fit_boot)
# a2b3 <- indirect_effect(x = "f2", y = "f4", m = "f3", fit = fit,
#                         boot_ci = TRUE, boot_out = fit_boot)
# confint(a1b3)
# confint(a2b3)
# a1b3std <- indirect_effect(x = "f1", y = "f4", m = "f3", fit = fit,
#                         boot_ci = TRUE, boot_out = fit_boot,
#                         standardized_x = TRUE, standardized_y = TRUE)
# a2b3std <- indirect_effect(x = "f2", y = "f4", m = "f3", fit = fit,
#                         boot_ci = TRUE, boot_out = fit_boot,
#                         standardized_x = TRUE, standardized_y = TRUE)
# confint(a1b3std)
# confint(a2b3std)

data_sem <- dat
usethis::use_data(data_sem, overwrite=TRUE)
