# To be examined in an interactive session

library(stdmodsem)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
dat <- add_na(dat, seed = 4589)
length(complete.cases(dat))
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, missing = "fiml")

out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)

out_1b <- cond_indirect_effects(wlevels = list("w4", c("gpgp2", "gpgp3")), x = "x", y = "y", m = "m3", fit = fit)

test_that("cond_indirect_effects: call mod_levels_list (lavaan)", {
    expect_equal(unlist(out_1), unlist(out_1b))
  })


out_mm_1 <- mod_levels_list("w4", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE, w_method = "percentile")

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit)

out_1b <- cond_indirect_effects(wlevels = list("w4", c("gpgp2", "gpgp3")), x = "x", y = "y", m = "m3", fit = fit, w_method = "percentile")

test_that("cond_indirect_effects: call mod_levels_list (lavaan), percentile", {
    expect_equal(unlist(out_1), unlist(out_1b))
  })


