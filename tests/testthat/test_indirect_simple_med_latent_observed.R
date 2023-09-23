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
