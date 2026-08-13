skip_on_cran()

library(manymome)
suppressMessages(library(lavaan))

test_that("confint.cond_indirect_effects", {

# From issue 299

set.seed(123)

dat <- data_med_mod_ab1
mod <- "
  m ~ x + w + x:w + c1 + c2
  y ~ m + w + x + c1 + c2
"
fit <- sem(mod, dat)

boot_out <- do_boot(fit, R = 200, seed = 123, parallel = FALSE, progress = !is_testing())

out_cond_95 <- cond_indirect_effects(
  wlevels = "w", x = "x", y = "y", m = "m", fit = fit,
  boot_ci = TRUE, boot_out = boot_out,
  level = .95
)

out_cond_99 <- cond_indirect_effects(
  wlevels = "w", x = "x", y = "y", m = "m", fit = fit,
  boot_ci = TRUE, boot_out = boot_out,
  level = .99
)

ci95_to_99 <- confint(out_cond_95, level = 0.99)
ci99_to_95 <- confint(out_cond_99)

expect_equal(
  ci99_to_95,
  confint(out_cond_95),
  ignore_attr = TRUE
)

expect_equal(
  ci95_to_99,
  confint(out_cond_99, level = .99),
  ignore_attr = TRUE
)

out_cond_95_1 <- attr(out_cond_95, "full_output")[[1]]
out_cond_99_1 <- attr(out_cond_99, "full_output")[[1]]

out_cond_95_1$boot_ci
out_cond_99_1$boot_ci

ci_1_95_to_99 <- confint(out_cond_95_1, level = .99)
ci_1_99_to_95 <- confint(out_cond_99_1, level = .95)

expect_equal(
  ci_1_95_to_99,
  confint(out_cond_99_1, level = .99),
  ignore_attr = TRUE
)

expect_equal(
  ci_1_99_to_95,
  confint(out_cond_95_1),
  ignore_attr = TRUE
)

out_cond_95_bc <- cond_indirect_effects(
  wlevels = "w", x = "x", y = "y", m = "m", fit = fit,
  boot_ci = TRUE, boot_out = boot_out,
  level = .95, boot_type = "bc"
)

ci99_to_95_bc <- confint(out_cond_99, level = 0.95, boot_type = "bc")

expect_equal(
  ci99_to_95_bc,
  confint(out_cond_95_bc, level = .95),
  ignore_attr = TRUE
)

})
