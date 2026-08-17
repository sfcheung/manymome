skip_on_cran()

library(manymome)
suppressMessages(library(lavaan))

test_that("confint.cond_indirect_effects", {

# From issue 299

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

out_cond_95bc <- cond_indirect_effects(
  wlevels = "w", x = "x", y = "y", m = "m", fit = fit,
  boot_ci = TRUE, boot_out = boot_out,
  level = .95,
  boot_type = "bc"
)


# ---- confint.cond_indirect_effects() ----

ci95_to_99 <- confint(out_cond_95, level = 0.99)
ci99_to_95 <- confint(out_cond_99, level = 0.95)
ci95_to_NULL <- confint(out_cond_95)
ci99_to_NULL <- confint(out_cond_99)

ci95_to_bc <- confint(out_cond_95, boot_type = "bc")

# Scenario: level not set (NULL)
expect_equal(
  out_cond_99[, c("CI.lo", "CI.hi")],
  ci99_to_NULL,
  ignore_attr = TRUE
)

# Scenario: level not set (NULL)
expect_equal(
  out_cond_95[, c("CI.lo", "CI.hi")],
  ci95_to_NULL,
  ignore_attr = TRUE
)

# Scenario: level set
# Checking attributes is intentional
expect_equal(
  ci99_to_95,
  confint(out_cond_95)
)

# Scenario: level set
# Checking attributes is intentional
expect_equal(
  ci95_to_99,
  confint(out_cond_99)
)

# Scenario: boot_type set
# Checking attributes is intentional
expect_equal(
  ci95_to_bc,
  confint(out_cond_95bc)
)

# ---- confint.cond_indirect_diff() ----

out_diff_99 <- cond_indirect_diff(
  out_cond_99,
  from = 2,
  to = 3,
  level = .99
)

out_diff_95 <- cond_indirect_diff(
  out_cond_95,
  from = 2,
  to = 3
)

out_diff_99_to_95 <- confint(out_diff_99, level = .95)
out_diff_95_to_99 <- confint(out_diff_95, level = .99)

out_diff_99_to_NULL <- confint(out_diff_99)
out_diff_95_to_NULL <- confint(out_diff_95)

# Scenario: level set
# Checking attributes is intentional
expect_equal(
  out_diff_99_to_95,
  confint(out_diff_95)
)

# Scenario: level set
# Checking attributes is intentional
expect_equal(
  out_diff_95_to_99,
  confint(out_diff_99)
)

# Scenario: level not set (NULL)
# Checking attributes is intentional
expect_equal(
  out_diff_99_to_NULL,
  out_diff_95_to_99
)

# Scenario: level not set (NULL)
# Checking attributes is intentional
expect_equal(
  out_diff_95_to_NULL,
  out_diff_99_to_95
)

# ---- confint.indirect() ----

out_cond_95_1 <- attr(out_cond_95, "full_output")[[1]]
out_cond_99_1 <- attr(out_cond_99, "full_output")[[1]]
out_cond_95bc_1 <- attr(out_cond_95bc, "full_output")[[1]]

ci_1_95_to_99 <- confint(out_cond_95_1, level = .99)
ci_1_99_to_95 <- confint(out_cond_99_1, level = .95)
ci_1_95_to_NULL <- confint(out_cond_95_1)
ci_1_99_to_NULL <- confint(out_cond_99_1)

ci_1_95_to_bc <- confint(out_cond_95_1, boot_type = "bc")

# Scenario: level set
# Checking attributes is intentional
expect_equal(
  ci_1_95_to_99,
  confint(out_cond_99_1)
)

# Scenario: level set
# Checking attributes is intentional
expect_equal(
  ci_1_99_to_95,
  confint(out_cond_95_1)
)

# Scenario: level not set (NULL)
# Checking attributes is intentional
expect_equal(
  ci_1_95_to_99,
  ci_1_99_to_NULL
)

# Scenario: level not set (NULL)
# Checking attributes is intentional
expect_equal(
  ci_1_99_to_95,
  ci_1_95_to_NULL
)

# Scenario: boot_type set
# Checking attributes is intentional
expect_equal(
  ci_1_95_to_bc,
  confint(out_cond_95bc_1)
)

out_cond_95_bc <- cond_indirect_effects(
  wlevels = "w", x = "x", y = "y", m = "m", fit = fit,
  boot_ci = TRUE, boot_out = boot_out,
  level = .95, boot_type = "bc"
)

ci99_to_95_bc <- confint(out_cond_99, level = 0.95, boot_type = "bc")

# Checking attributes is intentional
expect_equal(
  ci99_to_95_bc,
  confint(out_cond_95_bc, level = .95)
)

})
