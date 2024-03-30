skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x
y ~ m2 + m3 + x + w4 + x:w4
"

dat$xw4 <- dat$x * dat$w4
dat$m3w4 <- dat$m3 * dat$w4
mod2 <-
"
m3 ~ m1 + x
y ~ m2 + m3 + x + w4 + xw4 + w3 + m3:w3 + m3w4
"

# This model is not exactly identical to the previous one
# due the labelled variances
mod2_chk <-
"
m3 ~ m1 + c(a1, a2, a3)*x
y ~ m2 + c(b1, b2, b3)*m3 + x + w4 + xw4 + w3 + c(d31, d32, d33)*m3:w3 + c(d41, d42, d43)*m3w4
ab1 := a1*b1
ab2 := a2*b2
ab3 := a3*b3
ab2_d := a2*(b2 + 1*d32 + 2*d42)
ab1_d := a1*(b1 + 3*d31 + (-2)*d41)
x ~~ c(v_x1, v_x2, v_x3) * x
ab1_stdx := a1*b1*sqrt(v_x1)
ab2_stdx := a2*b2*sqrt(v_x2)
ab3_stdx := a3*b3*sqrt(v_x3)
"

dat$m3w3 <- dat$m3 * dat$w3
mod3 <-
"
m3 ~ m1 + x
y ~ m2 + m3 + x + w4 + xw4 + w3 + m3w3 + m3w4
"

mod_med <-
"
m1 ~ x
m2 ~ m1
y ~ m2 + x
"


# Check against lavaan

fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           group = "gp",
           group.label = c("gp3", "gp1", "gp2"))
fit_boot <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
                se = "boot", bootstrap = 5,
                warn = FALSE,
                group = "gp",
                group.label = c("gp3", "gp1", "gp2"),
                iseed = 2345)
do_boot_out <- fit2boot_out_do_boot(fit, R = 5,
                                    seed = 2345,
                                    progress = FALSE,
                                    parallel = FALSE)
lav_boot <- lavInspect(fit_boot, "boot")

test_that("Check against lavaan boot", {
    expect_equal(do_boot_out[[3]]$est$est[1:4],
                 unname(lav_boot[3, 1:4]))
  })

# get_implied_i_lavaan

fit_tmp <- sem(mod, dat[-c(1:10), ], meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))
my_implied <- get_implied_i(coef(fit), fit_tmp)
lav_implied <- lavInspect(fit, "implied")

test_that("Check against lavaan implied", {
    expect_equal(unclass(my_implied$cov$gp3),
                 unclass(lav_implied$gp3$cov))
  })

# get_prod

fit2 <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE,
            group = "gp",
            group.label = c("gp3", "gp1", "gp2"))
fit2_chk <- sem(mod2_chk, dat, meanstructure = TRUE, fixed.x = FALSE,
            group = "gp",
            group.label = c("gp3", "gp1", "gp2"))

fit2_ng <- sem(mod2, dat, meanstructure = TRUE, fixed.x = FALSE)
dat_tmp <- lav_data_used(fit2)
est_tmp <- lav_est(fit2, se = FALSE, ci = FALSE)
est_tmp2 <- est_tmp
est_tmp2$est <- est_tmp2$est * .5
est_tmp_ng <- lav_est(fit2_ng, se = FALSE, ci = FALSE)
est_tmp2_ng <- est_tmp_ng
est_tmp2_ng$est <- est_tmp2_ng$est * .5

test_that("get_prod and friends", {
    expect_true(setequal(c("x", "w4"),
                         find_product(dat_tmp, "xw4")))
    expect_true(setequal(names(find_all_products(dat_tmp)),
                         c("m3w4", "xw4")))
    tmp <- get_b(x = "xw4",
                 y = "y",
                 est = est_tmp)
    tmpchk <- est_tmp[(est_tmp$rhs == "xw4") &
                      (est_tmp$op == "~"), "est"]
    expect_equal(unname(tmp),
                 unname(tmpchk))
    tmp <- get_prod(x = "x",
                    y = "y",
                    fit = fit2,
                    expand = TRUE)
    expect_true(length(tmp$b$xw4) == 3)
  })

tmp1 <- get_prod(x = "x",
                 y = "y",
                 fit = fit2,
                 expand = TRUE)
tmp1_ng <- get_prod(x = "x",
                    y = "y",
                    fit = fit2_ng,
                    expand = TRUE)

# indirect_i

suppressWarnings(tmp2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = "gp1"))
suppressWarnings(tmp3 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 1))
# tmp2_chk <- est_tmp[(est_tmp$lhs == "m3") &
#                     (est_tmp$rhs == "x") &
#                     (est_tmp$group == 2), "est"] *
#             est_tmp[(est_tmp$lhs == "y") &
#                     (est_tmp$rhs == "m3") &
#                     (est_tmp$group == 2), "est"]
tmp2_chk <- coef(fit2_chk, type = "user")["ab2"]
# tmp3_chk <- est_tmp[(est_tmp$lhs == "m3") &
#                     (est_tmp$rhs == "x") &
#                     (est_tmp$group == 1), "est"] *
#             est_tmp[(est_tmp$lhs == "y") &
#                     (est_tmp$rhs == "m3") &
#                     (est_tmp$group == 1), "est"]
tmp3_chk <- coef(fit2_chk, type = "user")["ab1"]

test_that("indirect_effect and multigrop", {
    expect_equal(unname(coef(tmp2)),
                 unname(tmp2_chk),
                 tolerance = 1e-05)
    # Can't just compare them. Don't know why.
    expect_equal(unname(coef(tmp3)) - unname(tmp3_chk),
                 0)
  })

# cond_indirect

suppressWarnings(tmp2 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 1, w4 = 2),
                        group = 2))
suppressWarnings(tmp3 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 3, w4 = -2),
                        group = "gp3"))
# tmp2_chk <- est_tmp[(est_tmp$lhs == "m3") &
#                     (est_tmp$rhs == "x") &
#                     (est_tmp$group == 2), "est"] *
#             (est_tmp[(est_tmp$lhs == "y") &
#                      (est_tmp$rhs == "m3") &
#                      (est_tmp$group == 2), "est"] +
#              est_tmp[(est_tmp$lhs == "y") &
#                      (est_tmp$rhs == "m3:w3") &
#                      (est_tmp$group == 2), "est"] +
#              est_tmp[(est_tmp$lhs == "y") &
#                      (est_tmp$rhs == "m3w4") &
#                      (est_tmp$group == 2), "est"] * 2)
tmp2_chk <- coef(fit2_chk, type = "user")["ab2_d"]
# tmp3_chk <- est_tmp[(est_tmp$lhs == "m3") &
#                     (est_tmp$rhs == "x") &
#                     (est_tmp$group == 1), "est"] *
#             (est_tmp[(est_tmp$lhs == "y") &
#                      (est_tmp$rhs == "m3") &
#                      (est_tmp$group == 1), "est"] +
#              est_tmp[(est_tmp$lhs == "y") &
#                      (est_tmp$rhs == "m3:w3") &
#                      (est_tmp$group == 1), "est"] * 3+
#              est_tmp[(est_tmp$lhs == "y") &
#                      (est_tmp$rhs == "m3w4") &
#                      (est_tmp$group == 1), "est"] * -2)
tmp3_chk <- coef(fit2_chk, type = "user")["ab1_d"]

test_that("indirect_effect and multigrop", {
    expect_equal(unname(coef(tmp2)),
                 unname(tmp2_chk),
                 tolerance = 1e-5)
    expect_equal(unname(coef(tmp3)),
                 unname(tmp3_chk),
                 tolerance = 1e-4)
  })

# indirect_i: stdx / stdy

suppressWarnings(tmp2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = "gp1",
                        standardized_x = TRUE))
suppressWarnings(tmp3 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 3,
                        standardized_y = TRUE))
suppressWarnings(tmp4 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 3,
                        standardized_y = TRUE,
                        standardized_x = TRUE))
sd_x_2 <- sqrt(lavInspect(fit2, "implied")$gp1$cov["x", "x"])
sd_y_2 <- sqrt(lavInspect(fit2, "implied")$gp1$cov["y", "y"])
sd_x_3 <- sqrt(lavInspect(fit2, "implied")[[3]]$cov["x", "x"])
sd_y_3 <- sqrt(lavInspect(fit2, "implied")[[3]]$cov["y", "y"])
tmp2_chk <- est_tmp[(est_tmp$lhs == "m3") &
                    (est_tmp$rhs == "x") &
                    (est_tmp$group == 2), "est"] *
            est_tmp[(est_tmp$lhs == "y") &
                    (est_tmp$rhs == "m3") &
                    (est_tmp$group == 2), "est"] * sd_x_2 / 1
tmp3_chk <- est_tmp[(est_tmp$lhs == "m3") &
                    (est_tmp$rhs == "x") &
                    (est_tmp$group == 3), "est"] *
            est_tmp[(est_tmp$lhs == "y") &
                    (est_tmp$rhs == "m3") &
                    (est_tmp$group == 3), "est"] * 1 / sd_y_3

test_that("indirect_effect and multigrop", {
    expect_equal(unname(coef(tmp2)),
                 tmp2_chk)
    expect_equal(unname(coef(tmp3)),
                 tmp3_chk)
    expect_equal(unname(coef(tmp4)),
                 tmp3_chk * sd_x_3)
  })

## Math

suppressWarnings(tmp3b <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 2,
                        standardized_y = TRUE))
suppressWarnings(tmp3c <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 1,
                        standardized_y = TRUE))

tmpmath3a <- tmp3b - tmp3
tmpmath3b <- tmp3b + tmp3
tmpmath3c <- tmpmath3b - tmp3c
test_that("indirect_effect and multigrop: Math", {
    expect_equal(coef(tmpmath3a),
                 coef(tmp3b) - coef(tmp3))
    expect_equal(coef(tmpmath3b),
                 coef(tmp3b) + coef(tmp3))
    expect_equal(coef(tmpmath3c),
                 coef(tmp3b) + coef(tmp3) - coef(tmp3c))
  })

# cond_indirect: stdx / stdy

suppressWarnings(tmp2 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 1, w4 = 2),
                        group = 2,
                        standardized_x = TRUE))
suppressWarnings(tmp3 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 3, w4 = -2),
                        group = "gp3",
                        standardized_y = TRUE))
sd_x_2 <- sqrt(lavInspect(fit2, "implied")$gp1$cov["x", "x"])
sd_y_2 <- sqrt(lavInspect(fit2, "implied")$gp1$cov["y", "y"])
sd_x_3 <- sqrt(lavInspect(fit2, "implied")[[1]]$cov["x", "x"])
sd_y_3 <- sqrt(lavInspect(fit2, "implied")[[1]]$cov["y", "y"])
tmp2_chk <- est_tmp[(est_tmp$lhs == "m3") &
                    (est_tmp$rhs == "x") &
                    (est_tmp$group == 2), "est"] *
            (est_tmp[(est_tmp$lhs == "y") &
                     (est_tmp$rhs == "m3") &
                     (est_tmp$group == 2), "est"] +
             est_tmp[(est_tmp$lhs == "y") &
                     (est_tmp$rhs == "m3:w3") &
                     (est_tmp$group == 2), "est"] +
             est_tmp[(est_tmp$lhs == "y") &
                     (est_tmp$rhs == "m3w4") &
                     (est_tmp$group == 2), "est"] * 2) * sd_x_2
tmp3_chk <- est_tmp[(est_tmp$lhs == "m3") &
                    (est_tmp$rhs == "x") &
                    (est_tmp$group == 1), "est"] *
            (est_tmp[(est_tmp$lhs == "y") &
                     (est_tmp$rhs == "m3") &
                     (est_tmp$group == 1), "est"] +
             est_tmp[(est_tmp$lhs == "y") &
                     (est_tmp$rhs == "m3:w3") &
                     (est_tmp$group == 1), "est"] * 3+
             est_tmp[(est_tmp$lhs == "y") &
                     (est_tmp$rhs == "m3w4") &
                     (est_tmp$group == 1), "est"] * -2) * 1 / sd_y_3

test_that("indirect_effect and multigrop", {
    expect_equal(unname(coef(tmp2)),
                 tmp2_chk)
    expect_equal(unname(coef(tmp3)),
                 tmp3_chk)
  })

# Check do_mc

fit_eq <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
              group = "gp",
              group.label = c("gp3", "gp1", "gp2"),
              group.equal = "regressions")

fit_mc_out <- do_mc(fit, R = 4,
                   seed = 2345,
                   progress = FALSE,
                   parallel = FALSE)

fit_eq_mc_out <- do_mc(fit_eq, R = 4,
                       seed = 2345,
                       progress = FALSE,
                       parallel = FALSE)

fit_mc_out <- do_mc(fit2, R = 50,
                   seed = 2345,
                   progress = FALSE,
                   parallel = FALSE)

get_mc_est <- function(object, lhs, op = "~", rhs, group = NA) {
    out <- sapply(object, function(x) {
        esti <- x$est
        out1 <- esti[(esti$lhs == lhs) & (esti$op == op) & (esti$rhs == rhs), ]
        if (!is.na(group)) {
            out1 <- out1[out1$group == group, ]
          }
        out1[, "est"]
      })
    out
  }

get_mc_implied <- function(object, var, group = NA) {
    out <- sapply(object, function(x) {
        imp <- x$implied_stats$cov
        if (!is.na(group)) {
            out <- imp[[group]][var, var]
          } else {
            out <- imp[var, var]
          }
        out
      })
    out
  }

# indirect_i: mc

suppressWarnings(tmp2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = "gp1",
                        mc_ci = TRUE,
                        mc_out = fit_mc_out))
suppressWarnings(tmp3 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 1,
                        mc_ci = TRUE,
                        mc_out = fit_mc_out))

tmp2a <- get_mc_est(fit_mc_out, lhs = "m3", rhs = "x", group = 2)
tmp2b <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3", group = 2)
tmp2ab <- tmp2a * tmp2b
tmp3a <- get_mc_est(fit_mc_out, lhs = "m3", rhs = "x", group = 1)
tmp3b <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3", group = 1)
tmp3ab <- tmp3a * tmp3b

test_that("indirect_effect and multigrop", {
    expect_equal(tmp2$mc_indirect,
                 tmp2ab)
    expect_equal(tmp3$mc_indirect,
                 tmp3ab)
  })

# indirect_i: mc, stdx / stdy

suppressWarnings(tmp2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = "gp1",
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_y = TRUE))
suppressWarnings(tmp3 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 1,
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_x = TRUE))
suppressWarnings(tmp4 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 1,
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_y = TRUE,
                        standardized_x = TRUE))

tmp2a <- get_mc_est(fit_mc_out, lhs = "m3", rhs = "x", group = 2)
tmp2b <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3", group = 2)
tmp2ysd <- sqrt(get_mc_implied(fit_mc_out, var = "y", group = 2))
tmp2ab <- tmp2a * tmp2b / tmp2ysd
tmp3a <- get_mc_est(fit_mc_out, lhs = "m3", rhs = "x", group = 1)
tmp3b <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3", group = 1)
tmp3xsd <- sqrt(get_mc_implied(fit_mc_out, var = "x", group = 1))
tmp3ysd <- sqrt(get_mc_implied(fit_mc_out, var = "y", group = 1))
tmp3ab <- tmp3a * tmp3b * tmp3xsd
tmp4ab <- tmp3a * tmp3b * tmp3xsd / tmp3ysd

test_that("indirect_effect and multigrop", {
    expect_equal(tmp2$mc_indirect,
                 tmp2ab)
    expect_equal(tmp3$mc_indirect,
                 tmp3ab)
    expect_equal(tmp4$mc_indirect,
                 tmp4ab)
  })

## Math

suppressWarnings(tmp3b <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 3,
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_x = TRUE))

tmpmath3a <- tmp3b - tmp3
tmpmath3b <- tmp3b + tmp3
tmpmath3c <- tmpmath3b + tmpmath3a
test_that("indirect_effect and multigrop, MC: Math", {
    expect_equal(coef(tmpmath3a),
                 coef(tmp3b) - coef(tmp3))
    expect_equal(coef(tmpmath3b),
                 coef(tmp3b) + coef(tmp3))
  })


# cond_indirect, stdx / stdy: mc

suppressWarnings(tmp2 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 1, w4 = 2),
                        group = 2,
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_y = TRUE))
suppressWarnings(tmp3 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 3, w4 = -2),
                        group = "gp3",
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_x = TRUE))
suppressWarnings(tmp4 <- cond_indirect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        wvalues = c(w3 = 3, w4 = -2),
                        group = "gp3",
                        mc_ci = TRUE,
                        mc_out = fit_mc_out,
                        standardized_y = TRUE,
                        standardized_x = TRUE))

tmp2a <- get_mc_est(fit_mc_out, lhs = "m3", rhs = "x", group = 2)
tmp2b <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3", group = 2)
tmp2d1 <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3:w3", group = 2)
tmp2d2 <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3w4", group = 2)
tmp2ysd <- sqrt(get_mc_implied(fit_mc_out, var = "y", group = 2))
tmp2ab <- tmp2a * (tmp2b + 1 * tmp2d1 + 2 * tmp2d2) / tmp2ysd

tmp3a <- get_mc_est(fit_mc_out, lhs = "m3", rhs = "x", group = 1)
tmp3b <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3", group = 1)
tmp3d1 <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3:w3", group = 1)
tmp3d2 <- get_mc_est(fit_mc_out, lhs = "y", rhs = "m3w4", group = 1)
tmp3xsd <- sqrt(get_mc_implied(fit_mc_out, var = "x", group = 1))
tmp3ysd <- sqrt(get_mc_implied(fit_mc_out, var = "y", group = 1))
tmp3ab <- tmp3a * (tmp3b + 3 * tmp3d1 + (-2) * tmp3d2) * tmp3xsd

tmp4ab <- tmp3a * (tmp3b + 3 * tmp3d1 + (-2) * tmp3d2) * tmp3xsd / tmp3ysd

test_that("indirect_effect and multigrop", {
    expect_equal(tmp2$mc_indirect,
                 tmp2ab)
    expect_equal(tmp3$mc_indirect,
                 tmp3ab)
    expect_equal(tmp4$mc_indirect,
                 tmp4ab)
  })

# All direct paths

mod_tmp <-
"
m3 ~ c(NA, 0, NA)*m1
y ~ m3
"

fit_tmp <- sem(mod_tmp, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

test_that("All direct path: Multiple group", {
    expect_equal(all_indirect_paths(fit),
                 all_indirect_paths(fit2),
                 ignore_attr = TRUE)
    expect_equal(all_indirect_paths(fit, group = 2),
                 all_indirect_paths(fit2, group = "gp1"),
                 ignore_attr = TRUE)
    expect_true(length(all_indirect_paths(fit_tmp, group = 2)) == 0)
  })

# Many direct path

mod_tmp <-
"
m3 ~ c(NA, 0, NA)*m1 + c(NA, NA, 0)*x
m2 ~ c(0, 0, NA)*m1 + c(NA, NA, 0)*x
w3 ~ c(NA, 0, 0)*m2
y ~ c(NA, 0, NA)*m3 + w3
"
mod_tmp_2 <-
"
m3 ~ c(NA, 0, NA)*m1 + c(NA, NA, 0)*x
m2 ~ c(NA, NA, NA)*m1 + c(NA, NA, NA)*x
w3 ~ c(NA, NA, NA)*m2
y ~ c(0, NA, NA)*m3 + c(NA, NA, 0)*w3
"
mod_tmp_ng <-
"
m3 ~ m1 + x
m2 ~ m1 + x
w3 ~ m2
y ~ m3 + w3
"

fit_tmp <- sem(mod_tmp, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))
fit_tmp_2 <- sem(mod_tmp_2, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))
fit_tmp_ng <- sem(mod_tmp_ng, dat, meanstructure = TRUE, fixed.x = FALSE)

all_tmp <- all_indirect_paths(fit_tmp)
all_paths <- all_paths_to_df(all_tmp)
all_ind <- many_indirect_effects(all_tmp, fit = fit_tmp)
ind_chk <- indirect_effect(x = "x",
                           y = "w3",
                           m = "m2",
                           fit = fit_tmp,
                           group = "gp3")
all_ind_2 <- many_indirect_effects(all_indirect_paths(fit_tmp_2), fit = fit_tmp_2)
all_ind_ng <- many_indirect_effects(all_indirect_paths(fit_tmp_ng), fit = fit_tmp_ng)

test_that("many_indirect: multiple group", {
    expect_equal(coef(all_ind[[3]]),
                 coef(ind_chk))
  })

# indirect_effects_from_list

test_that("indirect_effects_from_list: multiple group", {
    expect_equal(unname(indirect_effects_from_list(all_ind)$ind),
                 unname(coef(all_ind)))
  })

# total_indirect_effect

test_that("total_indirect_effect: multiple group", {
    expect_equal(length(total_indirect_effect(all_ind_2, x = "x", y = "y")), 2)
    expect_error(total_indirect_effect(all_ind_2, x = "m3", y = "w3"))
  })

# Mediation only

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

tmp1 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med)
tmp1_chk1 <- indirect_effect(x = "x",
                             y = "y",
                             m = c("m1", "m2"),
                             fit = fit_med,
                             group = 1)
tmp1_chk2 <- indirect_effect(x = "x",
                             y = "y",
                             m = c("m1", "m2"),
                             fit = fit_med,
                             group = 2)
tmp1_chk3 <- indirect_effect(x = "x",
                             y = "y",
                             m = c("m1", "m2"),
                             fit = fit_med,
                             group = 3)

tmp2 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med,
                              groups = c(2, 1))

tmp3 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med,
                              groups = c("gp1", "gp3"))

test_that("cond_indirect_effects for multiple group", {
    expect_equal(unname(coef(tmp1)),
                 unname(c(coef(tmp1_chk1),
                          coef(tmp1_chk2),
                          coef(tmp1_chk3))))
    expect_equal(unname(coef(tmp2)),
                 unname(c(coef(tmp1_chk2),
                          coef(tmp1_chk1))))
    expect_equal(unname(coef(tmp3)),
                 unname(c(coef(tmp1_chk2),
                          coef(tmp1_chk1))))
    expect_error(tmp2 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med,
                              groups = c(10, 20)))
  })


# Group labels helpers

chk1 <- lavTech(fit2, "group.label")
test_that("group labels helpers", {
    expect_equal(group_labels_and_numbers(c(3, 1), fit2)$label,
                 chk1[c(3, 1)])
    expect_equal(group_labels_and_numbers(c("gp1", "gp3"), fit2)$number,
                 c(2, 1))
    expect_error(group_labels_and_numbers(c("gp5", "gp3"), fit2))
    expect_error(group_labels_and_numbers(10, fit2))
    expect_error(group_labels_and_numbers(1:2, "test"))
    expect_equal(group_labels_and_numbers(fit = fit2)$label,
                 chk1)
    expect_equal(group_labels_and_numbers(fit = fit2)$number,
                 seq_along(chk1))
  })

# print.cond_indirect_effects

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

tmp1 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med)

test_that("print.cond_indirect_effects: Multiple groups", {
    expect_output(print(tmp1),
                  "Conditional on group(s)",
                  fixed = TRUE)
  })


# coef.cond_indirect_effects

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

tmp1 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med)
tmp1_3 <- indirect_effect(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          fit = fit_med,
                          group = 3)

test_that("coef.cond_indirect_effects with multiple groups", {
    expect_equal(unname(coef(tmp1)[3]),
                 unname(coef(tmp1_3)))
  })

# [.cond_indirect_effects

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

tmp1 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med)

test_that("[.cond_indirect_effects: Multiple groups", {
    expect_equal(unname(coef(tmp1[c(1, 3), ])),
                 as.data.frame(tmp1)[c(1, 3), "ind"])
  })

# cond_indirect_diff

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

tmp1 <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med)

tmp1_diff <- cond_indirect_diff(tmp1,
                                from = 3,
                                to = 1)

tmp1_1 <- indirect_effect(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          fit = fit_med,
                          group = 1)
tmp1_3 <- indirect_effect(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          fit = fit_med,
                          group = 3)

test_that("cond_indirect_diff: Multiple groups", {
    expect_equal(unname(coef(tmp1_diff)),
                 unname(coef(tmp1_1) - coef(tmp1_3)))
  })

# plot.cond_indirect_effects

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))

tmp1 <- cond_indirect_effects(x = "m2",
                              y = "y",
                              fit = fit_med)

test_that("plot.cond_indirect_effects: multiple groups", {
    expect_no_error(p <- plot(tmp1))
    expect_true(setequal(unique(p$layers[[1]]$data$wlevels),
                         unique(tmp1$Group)))
  })





skip("Long tests: Test in interactive sections")

# Indirect with bootstrap

fit3 <- sem(mod3, dat, meanstructure = TRUE, fixed.x = FALSE,
            group = "gp",
            group.label = c("gp3", "gp1", "gp2"))

fit2_boot_out <- do_boot(fit2,
                         R = 50,
                         seed = 1234,
                         parallel = FALSE,
                         progress = FALSE)

suppressWarnings(fit2_chk_boot <- sem(mod2_chk, dat, meanstructure = TRUE, fixed.x = FALSE,
            group = "gp",
            group.label = c("gp3", "gp1", "gp2"),
            se = "bootstrap",
            bootstrap = 50,
            iseed = 1234))

fit2_chk_boot_out <- do_boot(fit2_chk_boot)

fit_med <- sem(mod_med, dat, meanstructure = TRUE, fixed.x = FALSE,
               group = "gp",
               group.label = c("gp3", "gp1", "gp2"))
fit_med_boot_out <- do_boot(fit_med,
                         R = 50,
                         seed = 1234,
                         parallel = FALSE,
                         progress = FALSE)

suppressWarnings(tmp2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = "gp1",
                        boot_ci = TRUE,
                        boot_out = fit2_boot_out))
suppressWarnings(tmp3 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 3,
                        boot_ci = TRUE,
                        boot_out = fit2_boot_out))

suppressWarnings(tmp2_chk_boot <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2_chk_boot,
                        group = "gp1",
                        boot_ci = TRUE))
suppressWarnings(tmp3_chk_boot <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2_chk_boot,
                        group = 3,
                        boot_ci = TRUE))

est_chk <- parameterEstimates(fit2_chk_boot)

test_that("indirect_effect and multigrop", {
    i <- match("ab2", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp2))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
    i <- match("ab3", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp3))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
    i <- match("ab2", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp2_chk_boot))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
    i <- match("ab3", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp3_chk_boot))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
  })

## Math

suppressWarnings(tmp3b <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 2,
                        boot_ci = TRUE,
                        boot_out = fit2_boot_out))

tmpmath3a <- tmp3b - tmp3
tmpmath3b <- tmp3b + tmp3
test_that("indirect_effect and multigrop, boot: Math", {
    expect_equal(coef(tmpmath3a),
                 coef(tmp3b) - coef(tmp3))
    expect_equal(coef(tmpmath3b),
                 coef(tmp3b) + coef(tmp3))
    expect_equal(tmpmath3a$boot_indirect,
                 tmp3b$boot_indirect - tmp3$boot_indirect)
    expect_equal(tmpmath3b$boot_indirect,
                 tmp3b$boot_indirect + tmp3$boot_indirect)
  })


# Indirect with bootstrap: stdx / stdy

suppressWarnings(tmp2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = "gp1",
                        boot_ci = TRUE,
                        boot_out = fit2_boot_out,
                        standardized_x = TRUE))
suppressWarnings(tmp3 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 3,
                        boot_ci = TRUE,
                        boot_out = fit2_boot_out,
                        standardized_x = TRUE))

suppressWarnings(tmp2_chk_boot <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2_chk_boot,
                        group = "gp1",
                        boot_ci = TRUE,
                        standardized_x = TRUE))
suppressWarnings(tmp3_chk_boot <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2_chk_boot,
                        group = 3,
                        boot_ci = TRUE,
                        standardized_x = TRUE))

est_chk <- parameterEstimates(fit2_chk_boot)

test_that("indirect_effect and multigrop", {
    i <- match("ab2_stdx", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp2))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
    i <- match("ab3_stdx", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp3))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
    i <- match("ab2_stdx", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp2_chk_boot))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
    i <- match("ab3_stdx", est_chk$lhs)
    expect_equal(unname(as.vector(confint(tmp3_chk_boot))),
                 unname(unlist(est_chk[i, c("ci.lower", "ci.upper")])),
                 tolerance = 1e-4)
  })

## Math

suppressWarnings(tmp3b <- indirect_effect(x = "x",
                        y = "y",
                        m = "m3",
                        fit = fit2,
                        group = 1,
                        boot_ci = TRUE,
                        boot_out = fit2_boot_out,
                        standardized_x = TRUE))

tmpmath3a <- tmp3b - tmp3
tmpmath3b <- tmp3b + tmp3
test_that("indirect_effect and multigrop, boot: Math", {
    expect_equal(coef(tmpmath3a),
                 coef(tmp3b) - coef(tmp3))
    expect_equal(coef(tmpmath3b),
                 coef(tmp3b) + coef(tmp3))
    expect_equal(tmpmath3a$boot_indirect,
                 tmp3b$boot_indirect - tmp3$boot_indirect)
    expect_equal(tmpmath3b$boot_indirect,
                 tmp3b$boot_indirect + tmp3$boot_indirect)
  })

# confint.cond_indirect_effects

tmp1_boot <- cond_indirect_effects(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              fit = fit_med,
                              boot_ci = TRUE,
                              boot_out = fit_med_boot_out)
suppressWarnings(tmp1_2 <- indirect_effect(x = "x",
                        y = "y",
                        m = c("m1", "m2"),
                        fit = fit_med,
                        group = 3,
                        boot_ci = TRUE,
                        boot_out = fit_med_boot_out))

tmp1_boot_ci <- confint(tmp1_boot)

test_that("confint.cond_indirect_effects with multiple groups", {
    expect_equal(unname(unlist(tmp1_boot_ci[3, ])),
                 unname(as.vector(confint(tmp1_2))))
  })

# indirect_effects_from_list

all_tmp <- all_indirect_paths(fit_med)
all_ind <- many_indirect_effects(all_tmp,
                                 fit = fit_med,
                                 boot_ci = TRUE,
                                 boot_out = fit_med_boot_out)

test_that("indirect_effects_from_list: multiple group", {
    expect_equal(unname(indirect_effects_from_list(all_ind)$ind),
                 unname(coef(all_ind)))
    expect_equal(unname(indirect_effects_from_list(all_ind)$`CI.lo`),
                 unname(confint(all_ind)[, 1]))
  })
