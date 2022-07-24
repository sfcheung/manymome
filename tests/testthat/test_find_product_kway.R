skip("WIP")
library(stdmodsem)
dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm_list <- lm2list(lm_m, lm_y)
library(lavaan)
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod <-
"
m ~ x + c1 + c2
y ~ m + x + w1 + w2 + w1m + w2m + w1w2 + w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit <- sem(mod, dat, fixed.x = FALSE, warn = FALSE)

get_prod("m", "y", est = lm2ptable(lm_list)$est,
                   data = lm2ptable(lm_list)$data,
                   expand = TRUE)

get_prod("m", "y", est = lm2ptable(lm_list)$est)

find_all_products(lm2ptable(lm_list)$data)

wvalues <- c(w1 = 5, w2 = 4)
chklm <- indirect(x = "x", y = "y", m = c("m"),
                      est = lm2ptable(lm_list)$est,
                      data = lm2ptable(lm_list)$data,
                      wvalues = wvalues,
                      expand = TRUE,
                      get_prods_only = TRUE)
chkla <- indirect(x = "x", y = "y", m = c("m"),
                      fit = fit,
                      wvalues = wvalues,
                      expand = TRUE,
                      get_prods_only = TRUE)

indlm <- indirect(x = "x", y = "y", m = c("m"),
                      est = lm2ptable(lm_list)$est,
                      data = lm2ptable(lm_list)$data,
                      wvalues = wvalues,
                      prods = chklm)

out_1 <- find_all_products(lavInspect(fit, "data"))
out_1_chk <- list(`m1:w2` = c("m1", "w2"), `m3:w4` = c("m3", "w4"), `x:w1` = c("x",
"w1"), `x:w3` = c("x", "w3"), `x:w4` = c("x", "w4"))


test_that("find_product", {
    expect_equal(
        out_1,
        out_1_chk
      )
  })