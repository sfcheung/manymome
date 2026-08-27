library(manymome)
suppressMessages(library(lavaan))

test_that("w not in the path", {

dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))
dat$xw1 <- dat$x * dat$w1
dat$xw2 <- dat$x * dat$w2
dat$w1w2 <- dat$w1 * dat$w2
dat$xw1w2 <- dat$x * dat$w1 * dat$w2

# A variable in wlevels is not a moderator

mod0 <-
"
m2 ~ x + w1 + w2 + xw1 + xw2 + w1w2 + xw1w2 + m1 + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
m3 ~ x + w1 + xw1
y ~ m2 + x + m3
"
fit0 <- sem(mod0, dat, meanstructure = TRUE, fixed.x = FALSE)

# One numeric

out_mm_1 <- mod_levels_list("m1", "w1", "w2", c("gpgp2", "gpgp3"), fit = fit0, merge = TRUE)
expect_warning(
  cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m2", fit = fit0),
  "failed"
)

# Numeric and categorical
expect_warning(
  cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "y", m = "m3", fit = fit0),
  "failed"
)

})
