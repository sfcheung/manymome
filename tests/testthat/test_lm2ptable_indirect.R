
library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 * w2, dat)
lm_m3 <- lm(m3 ~ m2 * w3, dat)
lm_y <- lm(y ~ m3 * w4 + x * w4, dat)

out <- lm2ptable(list(lm_m1, lm_m2, lm_m3, lm_y))

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

ce_1b_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), est = out$est,
                      wvalues = wvalues)
ce_1b_chk2 <- (coef(lm_m1)["x"] +
                wvalues["w1"] * coef(lm_m1)["x:w1"]) *
              (coef(lm_m2)["m1"] +
                wvalues["w2"] * coef(lm_m2)["m1:w2"]) *
              (coef(lm_m3)["m2"] +
                wvalues["w3"] * coef(lm_m3)["m2:w3"]) *
              (coef(lm_y)["m3"] +
                wvalues["w4"] * coef(lm_y)["m3:w4"])

ce_2_chk <- indirect_i(x = "x", y = "m1", est = out$est,
                      wvalues = wvalues)
ce_2_chk2 <- (coef(lm_m1)["x"] +
                wvalues["w1"] * coef(lm_m1)["x:w1"])

ce_3_chk <- indirect_i(x = "x", y = "y", est = out$est,
                      wvalues = wvalues["w4"])
ce_3_chk2 <- (coef(lm_y)["x"] +
                wvalues["w4"] * coef(lm_y)["w4:x"])

ce_1b_stdx_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), est = out$est,
                      wvalues = wvalues,
                      implied_stats = out$implied_stats,
                      standardized_x = TRUE)
ce_1b_stdy_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), est = out$est,
                      wvalues = wvalues,
                      implied_stats = out$implied_stats,
                      standardized_y = TRUE)
ce_1b_stdboth_chk <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), est = out$est,
                      wvalues = wvalues,
                      implied_stats = out$implied_stats,
                      standardized_x = TRUE,
                      standardized_y = TRUE)
sd_x <- sd(dat$x)
sd_y <- sd(dat$y)
ce_1b_stdx_chk2 <- ce_1b_chk2 * sd_x
ce_1b_stdy_chk2 <- ce_1b_chk2 / sd_y
ce_1b_stdboth_chk2 <- ce_1b_chk2 * sd_x / sd_y


test_that("lm2ptable with indirect", {
    expect_equal(unname(ce_1b_chk$indirect),
                 unname(ce_1b_chk2))
    expect_equal(unname(ce_2_chk$indirect),
                 unname(ce_2_chk2))
    expect_equal(unname(ce_3_chk$indirect),
                 unname(ce_3_chk2))
    expect_equal(unname(ce_1b_stdx_chk$indirect),
                 unname(ce_1b_stdx_chk2))
    expect_equal(unname(ce_1b_stdy_chk$indirect),
                 unname(ce_1b_stdy_chk2))
    expect_equal(unname(ce_1b_stdboth_chk$indirect),
                 unname(ce_1b_stdboth_chk2))
  })
