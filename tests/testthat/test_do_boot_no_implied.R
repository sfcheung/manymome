skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

test_that("do_boot: No implied", {

data(data_med_mod_ab1)
dat <- data_med_mod_ab1
mod <-
"
m ~ x + w + x:w + c1 + c2
y ~ m + w + x:w + x + c1 + c2
"
fit <- sem(mod, dat)

system.time(
boot_out1 <- do_boot(fit, R = 100, seed = 1234, parallel = FALSE)
)

system.time(
boot_out2 <- do_boot(fit, R = 100, seed = 1234, parallel = FALSE, compute_implied_stats = FALSE)
)

ind1 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       boot_ci = TRUE,
                       boot_out = boot_out1)
ind1

ind2 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       boot_ci = TRUE,
                       boot_out = boot_out2)
ind2

expect_equal(confint(ind1),
             confint(ind2))

std1 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       boot_ci = TRUE,
                       boot_out = boot_out1,
                       standardized_x = TRUE,
                       standardized_y = TRUE)

std2 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       boot_ci = TRUE,
                       boot_out = boot_out2,
                       standardized_x = TRUE,
                       standardized_y = TRUE)

expect_false(isFALSE(all.equal(confint(std1),
                               confint(std2))))

fit_implied <- lav_implied_all(fit)
x_sd <- sqrt(fit_implied$cov["c1", "c1"])
y_sd <- sqrt(fit_implied$cov["y", "y"])

expect_equal(confint(ind2) * x_sd / y_sd,
             confint(std2))

lm_m <- lm(m ~ x + w + x:w + c1 + c2, dat)
lm_y <- lm(y ~ m + w + x:w + x + c1 + c2, dat)
lm_all <- list(lm_m, lm_y)

system.time(
boot_out1 <- do_boot(lm_all, R = 200, seed = 1234, parallel = FALSE)
)

system.time(
boot_out2 <- do_boot(lm_all, R = 200, seed = 1234, parallel = FALSE, compute_implied_stats = FALSE)
)

ind1 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = lm_all,
                       boot_ci = TRUE,
                       boot_out = boot_out1)
ind1

ind2 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = lm_all,
                       boot_ci = TRUE,
                       boot_out = boot_out2)
ind2

expect_equal(confint(ind1),
             confint(ind2))

std1 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = lm_all,
                       boot_ci = TRUE,
                       boot_out = boot_out1,
                       standardized_x = TRUE,
                       standardized_y = TRUE)

# Standardized CIs cannot be formed (correclty) for lm in this case.

# std2 <- indirect_effect(x = "c1",
#                        m = "m",
#                        y = "y",
#                        fit = lm_all,
#                        boot_ci = TRUE,
#                        boot_out = boot_out2,
#                        standardized_x = TRUE,
#                        standardized_y = TRUE)

# expect_false(isFALSE(all.equal(confint(std1),
#                                confint(std2))))

# fit_implied <- lav_implied_all(fit)
# x_sd <- sqrt(fit_implied$cov["c1", "c1"])
# y_sd <- sqrt(fit_implied$cov["y", "y"])

# mm <- merge_model_matrix(lm_all)
# fit_vcov <- data2implied(mm)
# x_sd <- sqrt(fit_implied$cov["c1", "c1"])
# y_sd <- sqrt(fit_implied$cov["y", "y"])

# expect_equal(confint(ind2) * x_sd / y_sd,
#              confint(std2))

})