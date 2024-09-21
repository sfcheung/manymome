skip("WIP")
library(testthat)
library(manymome)

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm1_list <- lm2list(lm_m, lm_y)
lm2_list <- lm2list(lm_m)
lm3_list <- lm2list(lm_y)
lm1fit <- lm2ptable(lm1_list)
lm2fit <- lm2ptable(lm2_list)
lm3fit <- lm2ptable(lm3_list)
lm1fit$vcov <- lm_list_vcov(lm1_list)
lm2fit$vcov <- lm_list_vcov(lm2_list)
lm3fit$vcov <- lm_list_vcov(lm3_list)
lm1fit$df_residual <- lm_df_residual(lm1_list)
lm2fit$df_residual <- lm_df_residual(lm2_list)
lm3fit$df_residual <- lm_df_residual(lm3_list)

suppressMessages(suppressMessages(library(lavaan)))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod2 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
# Covariances not added. Not necessary for this test.
"
mod3 <-
"
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit2 <- sem(mod2, dat, fixed.x = FALSE, warn = FALSE)
fit3 <- sem(mod3, dat, fixed.x = FALSE, warn = FALSE)
est2 <- parameterEstimates(fit2)
est3 <- parameterEstimates(fit3)

wvalues <- c(w1 = 5, w2 = -4)

dat2 <- data_med_mod_b_mod
dat2$w1 <- dat2$w1 - wvalues["w1"]
dat2$w2 <- dat2$w2 - wvalues["w2"]
lm_m2 <- lm(m ~ x*w1 + c1 + c2, dat2)
lm_y2 <- lm(y ~ w1*m*w2 + x + c1 + c2, dat2)
x_cond <- coef(lm_m2)["x"]
x_cond_se <- sqrt(vcov(lm_m2)["x", "x"])
m_cond <- coef(lm_y2)["m"]
m_cond_se <- sqrt(vcov(lm_y2)["m", "m"])

# Moderation
ce_1a <- indirect_i(x = "x", y = "m",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov,
                    fit_df_residual = lm1fit$df_residual)
ce_1b <- indirect_i(x = "m", y = "y",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov,
                    fit_df_residual = lm1fit$df_residual)
ce_2 <- indirect_i(x = "x", y = "m",
                   fit = NULL,
                   est = lm2fit$est,
                   data = lm2fit$data,
                   implied_stats = lm2fit$implied_stats,
                   wvalues = wvalues,
                   est_vcov = lm2fit$vcov,
                   fit_df_residual = lm2fit$df_residual)
ce_3 <- indirect_i(x = "m", y = "y",
                   fit = NULL,
                   est = lm3fit$est,
                   data = lm3fit$data,
                   implied_stats = lm3fit$implied_stats,
                   wvalues = wvalues,
                   est_vcov = lm3fit$vcov,
                   fit_df_residual = lm3fit$df_residual)

test_that("Check SE and df", {
    expect_equal(
        coef(ce_1a),
        x_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(ce_1b),
        m_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1a$indirect_normal_se,
        x_cond_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$indirect_normal_se,
        m_cond_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1a$indirect_normal_se,
        ce_2$indirect_normal_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$indirect_normal_se,
        ce_3$indirect_normal_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1a$indirect_normal_df_residual,
        ce_2$indirect_normal_df_residual,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$indirect_normal_df_residual,
        ce_3$indirect_normal_df_residual,
        ignore_attr = TRUE
      )
  })
