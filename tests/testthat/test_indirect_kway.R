library(stdmodsem)

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm_list <- lm2list(lm_m, lm_y)
lm2fit <- lm2ptable(lm_list)

suppressMessages(library(lavaan))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit <- sem(mod, dat, fixed.x = FALSE, warn = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = -4)

# Moderated-moderated mediation
ce_1b_chk <- indirect(x = "x", y = "y", m = "m",
                      fit = fit,
                      wvalues = wvalues)
ce_1b_chk2 <- (est[est$label == "a", "est"] +
                wvalues["w1"] * est[est$label == "ad1", "est"]) *
              (est[est$label == "b", "est"] +
                wvalues["w1"] * est[est$label == "bd1", "est"] +
                wvalues["w2"] * est[est$label == "bd2", "est"] +
                wvalues["w1"] * wvalues["w2"] * est[est$label == "be12", "est"])

ce_2b_chk <- indirect(x = "x", y = "y", m = "m",
                      est = lm2fit$est,
                      data = lm2fit$data,
                      wvalues = wvalues)

test_that("check indirect: 3-way", {
    expect_equal(
        ce_1b_chk$indirect,
        ce_1b_chk2,
        ignore_attr = TRUE
      )
    expect_equal(
        ce_1b_chk$indirect,
        ce_2b_chk$indirect,
        ignore_attr = TRUE
      )
  })
