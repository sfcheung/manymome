skip_on_cran()

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


suppressMessages(suppressMessages(library(lavaan)))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod1 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
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
fit1 <- sem(mod1, dat, fixed.x = FALSE, warn = FALSE)
fit2 <- sem(mod2, dat, fixed.x = FALSE, warn = FALSE)
fit3 <- sem(mod3, dat, fixed.x = FALSE, warn = FALSE)

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

dat2$w1x <- dat2$w1 * dat2$x
dat2$w1m <- dat2$w1 * dat2$m
dat2$w2m <- dat2$w2 * dat2$m
dat2$w1w2 <- dat2$w1 * dat2$w2
dat2$w1mw2 <- dat2$w1 * dat2$m * dat2$w2
fit1_chk <- sem(mod1, dat2, fixed.x = FALSE, warn = FALSE)
fit2_chk <- sem(mod2, dat2, fixed.x = FALSE, warn = FALSE)
fit3_chk <- sem(mod3, dat2, fixed.x = FALSE, warn = FALSE)
x_scond <- coef(fit1_chk)["a"]
x_scond_se <- parameterEstimates(fit1_chk)[1, "se"]
m_scond <- coef(fit1_chk)["b"]
m_scond_se <- parameterEstimates(fit1_chk)[6, "se"]
m3_scond <- coef(fit3_chk)["b"]
m3_scond_se <- parameterEstimates(fit3_chk)[1, "se"]

# Moderation
ce_1a <- indirect_i(x = "x", y = "m",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov,
                    df_residual = lm1fit$df_residual)
ce_1b <- indirect_i(x = "m", y = "y",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov,
                    df_residual = lm1fit$df_residual)
ce_2 <- indirect_i(x = "x", y = "m",
                   fit = NULL,
                   est = lm2fit$est,
                   data = lm2fit$data,
                   implied_stats = lm2fit$implied_stats,
                   wvalues = wvalues,
                   est_vcov = lm2fit$vcov,
                   df_residual = lm2fit$df_residual)
ce_3 <- indirect_i(x = "m", y = "y",
                   fit = NULL,
                   est = lm3fit$est,
                   data = lm3fit$data,
                   implied_stats = lm3fit$implied_stats,
                   wvalues = wvalues,
                   est_vcov = lm3fit$vcov,
                   df_residual = lm3fit$df_residual)

co_1a <- cond_indirect(x = "x", y = "m",
                       fit = lm1_list,
                       wvalues = wvalues)
co_1b <- cond_indirect(x = "m", y = "y",
                       fit = lm1_list,
                       wvalues = wvalues)
co_2 <- cond_indirect(x = "x", y = "m",
                      fit = lm2_list,
                      wvalues = wvalues)
co_3 <- cond_indirect(x = "m", y = "y",
                      fit = lm3_list,
                      wvalues = wvalues)

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
        ce_1a$original_se,
        x_cond_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$original_se,
        m_cond_se,
        ignore_attr = TRUE
      )

    expect_equal(
        coef(co_1a),
        x_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(co_1b),
        m_cond,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1a$original_se,
        x_cond_se,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1b$original_se,
        m_cond_se,
        ignore_attr = TRUE
      )

    expect_equal(
        ce_1a$original_se,
        ce_2$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$original_se,
        ce_3$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1a$df_residual,
        ce_2$df_residual,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b$df_residual,
        ce_3$df_residual,
        ignore_attr = TRUE
      )

    expect_equal(
        co_1a$original_se,
        ce_2$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1b$original_se,
        ce_3$original_se,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1a$df_residual,
        ce_2$df_residual,
        ignore_attr = TRUE
      )
    expect_equal(
        co_1b$df_residual,
        ce_3$df_residual,
        ignore_attr = TRUE
      )

  })

# SEM

se_1a <- indirect_i(x = "x", y = "m",
                    fit = fit1,
                    est = lav_est(fit1),
                    implied_stats = lav_implied_all(fit1),
                    wvalues = wvalues,
                    est_vcov = get_vcov(fit1),
                    df_residual = lav_df_residual(fit1))

se_1b <- indirect_i(x = "m", y = "y",
                    fit = fit1,
                    est = lav_est(fit1),
                    implied_stats = lav_implied_all(fit1),
                    wvalues = wvalues,
                    est_vcov = get_vcov(fit1),
                    df_residual = lav_df_residual(fit1))

se_3 <- indirect_i(x = "m", y = "y",
                   fit = fit3,
                   est = lav_est(fit3),
                   implied_stats = lav_implied_all(fit3),
                   wvalues = wvalues,
                   est_vcov = get_vcov(fit3),
                   df_residual = lav_df_residual(fit3))

test_that("Check SE and df", {
    expect_equal(
        coef(se_1a),
        x_scond,
        ignore_attr = TRUE
      )
    expect_equal(
        coef(se_1b),
        m_scond,
        ignore_attr = TRUE
      )
    expect_equal(
        se_1a$original_se,
        x_scond_se,
        ignore_attr = TRUE
      )
    # # Difference due to the covariances omitted
    # expect_equal(
    #     se_1b$original_se,
    #     m_scond_se,
    #     ignore_attr = TRUE
    #   )

    expect_equal(
        coef(se_3),
        m_scond,
        ignore_attr = TRUE
      )
    expect_equal(
        se_3$original_se,
        m3_scond_se,
        ignore_attr = TRUE
      )
  })

# cond_indirect_effects

cos_1a <- cond_indirect_effects(wlevels = "w1",
                                x = "x", y = "m",
                                fit = lm1_list)
cos_1b <- cond_indirect_effects(wlevels = c("w1", "w2"),
                                x = "m", y = "y",
                                fit = lm1_list)
cos_2 <- cond_indirect_effects(wlevels = "w1",
                               x = "x", y = "m",
                               fit = lm2_list)
cos_3 <- cond_indirect_effects(wlevels = c("w1", "w2"),
                               x = "m", y = "y",
                               fit = lm3_list)

print(cos_1a,
      se = TRUE,
      pvalue = TRUE,
      se_ci = TRUE)

print(cos_1b,
      se = TRUE,
      pvalue = TRUE)
