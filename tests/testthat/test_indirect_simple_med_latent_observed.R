library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- simple_mediation_latent
dat$x <- rowMeans(dat[, c("x1", "x2", "x3")])
dat$m <- rowMeans(dat[, c("m1", "m2", "m3")])
dat$y <- rowMeans(dat[, c("y1", "y2", "y3")])

mod_x <-
"
# fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
fm ~ a * x
fy ~ b * fm + cp * x
indirect := a * b
"
fit_x <- sem(mod_x, dat, se = "none", baseline = FALSE)
est_x <- parameterEstimates(fit_x)
std_x <- standardizedSolution(fit_x)

mod_m <-
"
fx =~ x1 + x2 + x3
# fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
m ~ a * fx
fy ~ b * m + cp * fx
indirect := a * b
"
fit_m <- sem(mod_m, dat, se = "none", baseline = FALSE)
est_m <- parameterEstimates(fit_m)
std_m <- standardizedSolution(fit_m)

mod_y <-
"
fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
# fy =~ y1 + y2 + y3
fm ~ a * fx
y ~ b * fm + cp * fx
indirect := a * b
"
fit_y <- sem(mod_y, dat, se = "none", baseline = FALSE)
est_y <- parameterEstimates(fit_y)
std_y <- standardizedSolution(fit_y)

mod_xm <-
"
# fx =~ x1 + x2 + x3
# fm =~ m1 + m2 + m3
fy =~ y1 + y2 + y3
m ~ a * x
fy ~ b * m + cp * x
indirect := a * b
"
fit_xm <- sem(mod_xm, dat, se = "none", baseline = FALSE)
est_xm <- parameterEstimates(fit_xm)
std_xm <- standardizedSolution(fit_xm)

mod_xy <-
"
# fx =~ x1 + x2 + x3
fm =~ m1 + m2 + m3
# fy =~ y1 + y2 + y3
fm ~ a * x
y ~ b * fm + cp * x
indirect := a * b
"
fit_xy <- sem(mod_xy, dat, se = "none", baseline = FALSE)
est_xy <- parameterEstimates(fit_xy)
std_xy <- standardizedSolution(fit_xy)

mod_my <-
"
fx =~ x1 + x2 + x3
# fm =~ m1 + m2 + m3
# fy =~ y1 + y2 + y3
m ~ a * fx
y ~ b * m + cp * fx
indirect := a * b
"
fit_my <- sem(mod_my, dat, se = "none", baseline = FALSE)
est_my <- parameterEstimates(fit_my)
std_my <- standardizedSolution(fit_my)

out_x <- indirect_i(x = "x", y = "fy", m = c("fm"), fit = fit_x,
                    allow_mixing_lav_and_obs = TRUE)
out_x
out_m <- indirect_i(x = "fx", y = "fy", m = c("m"), fit = fit_m,
                    allow_mixing_lav_and_obs = TRUE)
out_m
out_y <- indirect_i(x = "fx", y = "y", m = c("fm"), fit = fit_y,
                    allow_mixing_lav_and_obs = TRUE)
out_y

out_xm <- indirect_i(x = "x", y = "fy", m = c("m"), fit = fit_xm,
                    allow_mixing_lav_and_obs = TRUE)
out_xm
out_xy <- indirect_i(x = "x", y = "y", m = c("fm"), fit = fit_xy,
                     allow_mixing_lav_and_obs = TRUE)
out_xy
out_my <- indirect_i(x = "fx", y = "y", m = c("m"), fit = fit_my,
                    allow_mixing_lav_and_obs = TRUE)
out_my


out_std_x <- indirect_i(x = "x", y = "fy", m = c("fm"), fit = fit_x,
                    standardized_x = TRUE,
                    standardized_y = TRUE,
                    allow_mixing_lav_and_obs = TRUE)
out_std_x
out_std_m <- indirect_i(x = "fx", y = "fy", m = c("m"), fit = fit_m,
                    standardized_x = TRUE,
                    standardized_y = TRUE,
                    allow_mixing_lav_and_obs = TRUE)
out_std_m
out_std_y <- indirect_i(x = "fx", y = "y", m = c("fm"), fit = fit_y,
                    standardized_x = TRUE,
                    standardized_y = TRUE,
                    allow_mixing_lav_and_obs = TRUE)
out_std_y

out_std_xm <- indirect_i(x = "x", y = "fy", m = c("m"), fit = fit_xm,
                    standardized_x = TRUE,
                    standardized_y = TRUE,
                    allow_mixing_lav_and_obs = TRUE)
out_std_xm
out_std_xy <- indirect_i(x = "x", y = "y", m = c("fm"), fit = fit_xy,
                    standardized_x = TRUE,
                    standardized_y = TRUE,
                    allow_mixing_lav_and_obs = TRUE)
out_std_xy
out_std_my <- indirect_i(x = "fx", y = "y", m = c("m"), fit = fit_my,
                    standardized_x = TRUE,
                    standardized_y = TRUE,
                    allow_mixing_lav_and_obs = TRUE)
out_std_my


test_that("indirect: latent variable and observed variables", {
    expect_equal(out_x$indirect,
                 est_x[19, "est"])
    expect_equal(out_m$indirect,
                 est_m[19, "est"])
    expect_equal(out_y$indirect,
                 est_y[19, "est"])
    expect_equal(out_xm$indirect,
                 est_xm[13, "est"])
    expect_equal(out_xy$indirect,
                 est_xy[13, "est"])
    expect_equal(out_my$indirect,
                 est_my[13, "est"])
    expect_equal(out_std_x$indirect,
                 std_x[19, "est.std"])
    expect_equal(out_std_m$indirect,
                 std_m[19, "est.std"])
    expect_equal(out_std_y$indirect,
                 std_y[19, "est.std"])
    expect_equal(out_std_xm$indirect,
                 std_xm[13, "est.std"])
    expect_equal(out_std_xy$indirect,
                 std_xy[13, "est.std"])
    expect_equal(out_std_my$indirect,
                 std_my[13, "est.std"])
  })

skip_on_cran()

# Bootstrapping

fit_x_boot <- sem(mod_x, dat,
                  se = "bootstrap", bootstrap = 50, baseline = FALSE,
                  h1 = FALSE, warn = FALSE,
                  iseed = 85701)
boot_x_out <- suppressMessages(fit2boot_out_do_boot(fit_x, R = 50, seed = 85701,
                                                    progress = FALSE))
boot_x_est <- lapply(boot_x_out, function(x) x$est)
boot_x_implied_stats <- lapply(boot_x_out, function(x) x$implied_stats)

out_x_cond_boot <- cond_indirect(x = "x", y = "fy", m = c("fm"), fit = fit_x,
                                 boot_ci = TRUE,
                                 boot_out = boot_x_out)
out_x_cond_boot_chk <- mapply(indirect_i,
                              est = boot_x_est,
                              implied_stats = boot_x_implied_stats,
                              MoreArgs = list(x = "x",
                                              y = "fy",
                                              m = "fm"),
                              SIMPLIFY = FALSE)

fit_m_boot <- sem(mod_m, dat,
                  se = "bootstrap", bootstrap = 50, baseline = FALSE,
                  h1 = FALSE, warn = FALSE,
                  iseed = 85701)
boot_m_out <- suppressMessages(fit2boot_out_do_boot(fit_m, R = 50, seed = 85701,
                                                    progress = FALSE))
boot_m_est <- lapply(boot_m_out, function(x) x$est)
boot_m_implied_stats <- lapply(boot_m_out, function(x) x$implied_stats)

out_m_cond_boot <- cond_indirect(x = "fx", y = "fy", m = c("m"), fit = fit_m,
                                 boot_ci = TRUE,
                                 boot_out = boot_m_out)
out_m_cond_boot_chk <- mapply(indirect_i,
                              est = boot_m_est,
                              implied_stats = boot_m_implied_stats,
                              MoreArgs = list(x = "fx",
                                              y = "fy",
                                              m = "m"),
                              SIMPLIFY = FALSE)

fit_y_boot <- sem(mod_y, dat,
                  se = "bootstrap", bootstrap = 50, baseline = FALSE,
                  h1 = FALSE, warn = FALSE,
                  iseed = 85701)
boot_y_out <- suppressMessages(fit2boot_out_do_boot(fit_y, R = 50, seed = 85701,
                                                    progress = FALSE))
boot_y_est <- lapply(boot_y_out, function(x) x$est)
boot_y_implied_stats <- lapply(boot_y_out, function(x) x$implied_stats)

out_y_cond_boot <- cond_indirect(x = "fx", y = "y", m = c("fm"), fit = fit_y,
                                 boot_ci = TRUE,
                                 boot_out = boot_y_out)
out_y_cond_boot_chk <- mapply(indirect_i,
                              est = boot_y_est,
                              implied_stats = boot_m_implied_stats,
                              MoreArgs = list(x = "fx",
                                              y = "y",
                                              m = "fm"),
                              SIMPLIFY = FALSE)

fit_xm_boot <- sem(mod_xm, dat,
                   se = "bootstrap", bootstrap = 50, baseline = FALSE,
                   h1 = FALSE, warn = FALSE,
                   iseed = 85701)
boot_xm_out <- suppressMessages(fit2boot_out_do_boot(fit_xm, R = 50, seed = 85701,
                                                     progress = FALSE))
boot_xm_est <- lapply(boot_xm_out, function(x) x$est)
boot_xm_implied_stats <- lapply(boot_xm_out, function(x) x$implied_stats)

out_xm_cond_boot <- cond_indirect(x = "x", y = "fy", m = c("m"), fit = fit_xm,
                                 boot_ci = TRUE,
                                 boot_out = boot_xm_out)
out_xm_cond_boot_chk <- mapply(indirect_i,
                              est = boot_xm_est,
                              implied_stats = boot_xm_implied_stats,
                              MoreArgs = list(x = "x",
                                              y = "fy",
                                              m = "m"),
                              SIMPLIFY = FALSE)

fit_xy_boot <- sem(mod_xy, dat,
                  se = "bootstrap", bootstrap = 50, baseline = FALSE,
                  h1 = FALSE, warn = FALSE,
                  iseed = 85701)
boot_xy_out <- suppressMessages(fit2boot_out_do_boot(fit_xy, R = 50, seed = 85701,
                                                    progress = FALSE))
boot_xy_est <- lapply(boot_xy_out, function(x) x$est)
boot_xy_implied_stats <- lapply(boot_xy_out, function(x) x$implied_stats)

out_xy_cond_boot <- cond_indirect(x = "x", y = "y", m = c("fm"), fit = fit_xy,
                                 boot_ci = TRUE,
                                 boot_out = boot_xy_out)
out_xy_cond_boot_chk <- mapply(indirect_i,
                              est = boot_xy_est,
                              implied_stats = boot_xy_implied_stats,
                              MoreArgs = list(x = "x",
                                              y = "y",
                                              m = "fm"),
                              SIMPLIFY = FALSE)

fit_my_boot <- sem(mod_my, dat,
                  se = "bootstrap", bootstrap = 50, baseline = FALSE,
                  h1 = FALSE, warn = FALSE,
                  iseed = 85701)
boot_my_out <- suppressMessages(fit2boot_out_do_boot(fit_my, R = 50, seed = 85701,
                                                    progress = FALSE))
boot_my_est <- lapply(boot_my_out, function(x) x$est)
boot_my_implied_stats <- lapply(boot_my_out, function(x) x$implied_stats)

out_my_cond_boot <- cond_indirect(x = "fx", y = "y", m = c("m"), fit = fit_my,
                                 boot_ci = TRUE,
                                 boot_out = boot_my_out)
out_my_cond_boot_chk <- mapply(indirect_i,
                              est = boot_my_est,
                              implied_stats = boot_my_implied_stats,
                              MoreArgs = list(x = "fx",
                                              y = "y",
                                              m = "m"),
                              SIMPLIFY = FALSE)

test_that("indirect: latent variable and observed variables, boot", {
    expect_equal(out_x_cond_boot$boot_indirect,
                 sapply(out_x_cond_boot_chk, function(x) x$indirect))
    expect_equal(out_m_cond_boot$boot_indirect,
                 sapply(out_m_cond_boot_chk, function(x) x$indirect))
    expect_equal(out_y_cond_boot$boot_indirect,
                 sapply(out_y_cond_boot_chk, function(x) x$indirect))
    expect_equal(out_xm_cond_boot$boot_indirect,
                 sapply(out_xm_cond_boot_chk, function(x) x$indirect))
    expect_equal(out_xy_cond_boot$boot_indirect,
                 sapply(out_xy_cond_boot_chk, function(x) x$indirect))
    expect_equal(out_my_cond_boot$boot_indirect,
                 sapply(out_my_cond_boot_chk, function(x) x$indirect))
  })
