skip_on_cran()

library(manymome)
suppressMessages(library(lavaan))

test_that("confint.indirect_list", {

dat <- data_med_mod_ab1
mod <- "
  m ~ x + c1
  w ~ x
  c2 ~ w
  y ~ m + c2 + w + x
"
fit <- sem(mod, dat)

boot_out <- do_boot(fit, R = 200, seed = 123, parallel = FALSE, progress = !is_testing())

all_paths <- all_indirect_paths(
  fit = fit,
  x = "x",
  y = "y"
)

out_95 <- many_indirect_effects(
  all_paths,
  fit = fit,
  boot_out = boot_out,
  boot_ci = TRUE,
  level = .95
)

out_99 <- many_indirect_effects(
  all_paths,
  fit = fit,
  boot_out = boot_out,
  boot_ci = TRUE,
  level = .99
)

out_95bc <- many_indirect_effects(
  all_paths,
  fit = fit,
  boot_out = boot_out,
  boot_ci = TRUE,
  level = .95,
  boot_type = "bc"
)


# ---- confint.indirect_list() ----

ci95_to_99 <- confint(out_95, level = 0.99)
ci99_to_95 <- confint(out_99, level = 0.95)
ci95_to_NULL <- confint(out_95)
ci99_to_NULL <- confint(out_99)
ci95_to_bc <- confint(out_95, boot_type = "bc")

# Scenario: level not set (NULL) vs. level set
expect_equal(
  ci99_to_NULL,
  ci95_to_99
)

# Scenario: level not set (NULL) vs. level set
expect_equal(
  ci95_to_NULL,
  ci99_to_95
)

# Scenario: boot_type set
expect_equal(
  ci95_to_bc,
  confint(out_95bc)
)

})
