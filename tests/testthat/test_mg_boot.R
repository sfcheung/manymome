skip("WIP")

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

indirect_effect(x = "x",
                y = "y",
                m = "m3",
                fit = fit2,
                group_number = 2)
indirect_effect(x = "x",
                y = "y",
                m = "m3",
                fit = fit2_ng)
