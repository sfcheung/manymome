library(manymome)
dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm_list <- lm2list(lm_m, lm_y)
suppressMessages(library(lavaan))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod <-
"
m ~ x + w1 + w1x + c1 + c2
y ~ m + x + w1 + w2 + w1m + w2m + w1w2 + w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit <- sem(mod, dat, fixed.x = FALSE, warn = FALSE)

chk1 <- get_prod("m", "y", est = lm2ptable(lm_list)$est,
                           data = lm2ptable(lm_list)$data,
                           expand = TRUE)
chk1ans <- list(prod = c("w1:m", "m:w2", "w1:m:w2"),
  b = c(`w1:m` = -1.23202128450712, `m:w2` = -1.98778672890926, `w1:m:w2` = 0.396738743601429),
  w = list("w1", "w2", c("w1", "w2")), x = "m", y = "y")

chk2 <- get_prod("m", "y", fit = fit,
                           expand = TRUE)
chk2ans <- list(prod = c("w1m", "w2m", "w1mw2"),
 b = c(w1m = -1.23202128450741, w2m = -1.98778672890975, w1mw2 = 0.396738743601522),
 w = list("w1", "w2", c("w1", "w2")), x = "m", y = "y")

chk3 <- get_prod("x", "m", est = lm2ptable(lm_list)$est,
                           data = lm2ptable(lm_list)$data,
                           expand = TRUE)
chk3ans <- list(prod = "x:w1",
  b = c(`x:w1` = 0.0555871134592859),
  w = "w1", x = "x", y = "m")

chk4 <- get_prod("x", "m", fit = fit,
                           expand = TRUE)
chk4ans <- list(prod = "w1x",
  b = c(w1x = 0.0555871134592891),
  w = "w1", x = "x", y = "m")

test_that("get_prod for moderated-moderated-mediation: Output", {
    expect_equal(
        chk1,
        chk1ans
      )
    expect_equal(
        chk2,
        chk2ans
      )
    expect_equal(
        chk3,
        chk3ans
      )
    expect_equal(
        chk4,
        chk4ans
      )
  })


test_that("get_prod for moderated-moderated-mediation: lm vs lavaan", {
    expect_equal(
        chk1$b,
        chk2$b,
        ignore_attr = TRUE
      )
    expect_equal(
        chk3$b,
        chk4$b,
        ignore_attr = TRUE
      )
  })
