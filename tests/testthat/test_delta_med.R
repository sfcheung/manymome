library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Simple Mediation

dat <- data_med
mod <-
"
m ~ a * x
y ~ b * m + cp * x
ab := a * b
"
fit <- sem(mod, dat, fixed.x = FALSE)

# By Liu et al. (2023), Eq. 23

delta_med1 <- function(fit,
                       x = "x",
                       m = "m",
                       y = "y") {
    est <- lavaan::parameterEstimates(fit)
    implied_cov <- lavaan::lavInspect(fit, "implied")$cov
    a <- est[est$lhs == m & est$rhs == x, "est"]
    b <- est[est$lhs == y & est$rhs == m, "est"]
    cp <- est[est$lhs == y & est$rhs == x, "est"]
    var_x <- diag(implied_cov)[x]
    var_y <- diag(implied_cov)[y]
    out <- ((a^2) * (b^2) + 2 * a * b * cp) * var_x / var_y
    unname(out)
  }

dm_liu <- delta_med1(fit)

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = "m", y = "y")

test_that("Simple mediation", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })

# Parallel Mediation

dat <- data_parallel
mod <-
"
m1 ~ a1 * x
m2 ~ a2 * x
y ~ b2 * m2 + b1 * m1 + x
m1 ~~ m2
indirect1 := a1 * b1
indirect2 := a2 * b2
indirect := a1 * b1 + a2 * b2
"
fit <- sem(mod, dat, fixed.x = FALSE)

# By Liu et al. (2023), Eqs. 30 - 32

lm_m1 <- lm(m1 ~ x, dat)
lm_m2 <- lm(m2 ~ x, dat)
lm_y <- lm(y ~ m1 + m2 + x, dat)

e_m1 <- residuals(lm_m1)
e_m2 <- residuals(lm_m2)

b1 <- coef(lm_y)["m1"]
b2 <- coef(lm_y)["m2"]
cp <- coef(lm_y)["x"]
d_y <- coef(lm_y)["(Intercept)"]
y_hat_0 <- d_y + cp * dat$x + b1 * e_m1 + b2 * e_m2
rsq0_y1 <- var(y_hat_0) / var(dat$y)
y_hat_1 <- d_y + cp * dat$x + b1 * dat$m1 + b2 * dat$m2
rsq1_y1 <- var(y_hat_1) / var(dat$y)
rsq_y <- summary(lm_y)$r.square

dm_liu <- rsq_y - rsq0_y1

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = c("m1", "m2"), y = "y")

test_that("Parallel Mediation", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })

# m1 only

# By Liu et al. (2023), Eqs. 30 - 32

lm_m1 <- lm(m1 ~ x, dat)
lm_m2 <- lm(m2 ~ x, dat)
lm_y <- lm(y ~ m1 + m2 + x, dat)

e_m1 <- residuals(lm_m1)
e_m2 <- residuals(lm_m2)

b1 <- coef(lm_y)["m1"]
b2 <- coef(lm_y)["m2"]
cp <- coef(lm_y)["x"]
d_y <- coef(lm_y)["(Intercept)"]
y_hat_0 <- d_y + cp * dat$x + b1 * e_m1 + b2 * dat$m2
rsq0_y1 <- var(y_hat_0) / var(dat$y)
y_hat_1 <- d_y + cp * dat$x + b1 * dat$m1 + b2 * dat$m2
rsq1_y1 <- var(y_hat_1) / var(dat$y)
rsq_y <- summary(lm_y)$r.square

dm_liu <- rsq_y - rsq0_y1

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = c("m1"), y = "y")

test_that("Parallel Mediation: m1", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })

# m2 only

# By Liu et al. (2023), Eqs. 30 - 32

lm_m1 <- lm(m1 ~ x, dat)
lm_m2 <- lm(m2 ~ x, dat)
lm_y <- lm(y ~ m1 + m2 + x, dat)

e_m1 <- residuals(lm_m1)
e_m2 <- residuals(lm_m2)

b1 <- coef(lm_y)["m1"]
b2 <- coef(lm_y)["m2"]
cp <- coef(lm_y)["x"]
d_y <- coef(lm_y)["(Intercept)"]
y_hat_0 <- d_y + cp * dat$x + b1 * dat$m1 + b2 * e_m2
rsq0_y1 <- var(y_hat_0) / var(dat$y)
y_hat_1 <- d_y + cp * dat$x + b1 * dat$m1 + b2 * dat$m2
rsq1_y1 <- var(y_hat_1) / var(dat$y)
rsq_y <- summary(lm_y)$r.square

dm_liu <- rsq_y - rsq0_y1

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = c("m2"), y = "y")

test_that("Parallel Mediation: m1", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })

# Serial Mediation

dat <- data_serial
mod <-
"
m1 ~ a * x
m2 ~ b1 * m1 + x
y ~ b2 * m2 + cp2 * m1 + cp1 * x
indirect := a * b1 * b2
"
fit <- sem(mod, dat, fixed.x = FALSE)

# By Liu et al. (2023)

lm_m1 <- lm(m1 ~ x, dat)
lm_m2 <- lm(m2 ~ m1 + x, dat)
lm_y <- lm(y ~ m1 + m2 + x, dat)

e_m1 <- residuals(lm_m1)
e_m2 <- residuals(lm_m2)

cp2 <- coef(lm_y)["m1"]
b2 <- coef(lm_y)["m2"]
cp <- coef(lm_y)["x"]
d_y <- coef(lm_y)["(Intercept)"]
y_hat_0 <- d_y + cp * dat$x + cp2 * e_m1 + b2 * e_m2
rsq0_y1 <- var(y_hat_0) / var(dat$y)
y_hat_1 <- d_y + cp * dat$x + cp2 * dat$m1 + b2 * dat$m2
rsq1_y1 <- var(y_hat_1) / var(dat$y)
rsq_y <- summary(lm_y)$r.square

dm_liu <- rsq_y - rsq0_y1

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = c("m1", "m2"), y = "y")

test_that("Serial Mediation", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })

# Serial and through m1 only

# By Liu et al. (2023)

lm_m1 <- lm(m1 ~ x, dat)
lm_m2 <- lm(m2 ~ m1 + x, dat)
lm_y <- lm(y ~ m1 + m2 + x, dat)

e_m1 <- residuals(lm_m1)
e_m2 <- residuals(lm_m2)

a <- coef(lm_m1)["x"]
b1 <- coef(lm_m2)["m1"]
cp2 <- coef(lm_y)["m1"]
b2 <- coef(lm_y)["m2"]
cp <- coef(lm_y)["x"]
d_y <- coef(lm_y)["(Intercept)"]
dat$m2p <- dat$m2 - a * b1 * dat$x
y_hat_0 <- d_y + cp * dat$x + cp2 * e_m1 + b2 * dat$m2p
rsq0_y1 <- var(y_hat_0) / var(dat$y)
y_hat_1 <- d_y + cp * dat$x + cp2 * dat$m1 + b2 * dat$m2p
rsq1_y1 <- var(y_hat_1) / var(dat$y)
rsq_y <- summary(lm_y)$r.square

dm_liu <- rsq_y - rsq0_y1

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = c("m1"), y = "y")

test_that("Serial Mediation: m1", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })

# Serial and through m2 only

# By Liu et al. (2023)

lm_m1 <- lm(m1 ~ x, dat)
lm_m2 <- lm(m2 ~ m1 + x, dat)
lm_y <- lm(y ~ m1 + m2 + x, dat)

e_m1 <- residuals(lm_m1)
e_m2 <- residuals(lm_m2)

a <- coef(lm_m1)["x"]
b1 <- coef(lm_m2)["m1"]
cp2 <- coef(lm_y)["m1"]
b2 <- coef(lm_y)["m2"]
cp <- coef(lm_y)["x"]
d_y <- coef(lm_y)["(Intercept)"]
y_hat_0 <- d_y + cp * dat$x + cp2 * dat$m1 + b2 * e_m2
rsq0_y1 <- var(y_hat_0) / var(dat$y)
y_hat_1 <- d_y + cp * dat$x + cp2 * dat$m1 + b2 * dat$m2
rsq1_y1 <- var(y_hat_1) / var(dat$y)
rsq_y <- summary(lm_y)$r.square

dm_liu <- rsq_y - rsq0_y1

# By setting parameters

dm_my <- delta_med(fit, x = "x", m = c("m2"), y = "y")

test_that("Serial Mediation: m2", {
    expect_equal(coef(dm_my),
                 dm_liu)
  })


