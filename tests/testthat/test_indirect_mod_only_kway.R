skip("WIP")
library(testthat)
library(manymome)

dat <- data_med_mod_b_mod
lm_m <- lm(m ~ x*w1 + c1 + c2, dat)
lm_y <- lm(y ~ w1*m*w2 + x + c1 + c2, dat)
lm1_list <- lm2list(lm_m, lm_y)
lm2_list <- lm2list(lm_m)
lm3_list <- lm2list(lm_y)
lm1fit <- lm2ptable(lm1_list)
lm2fit <- lm2ptable(lm2_list)
lm3fit <- lm2ptable(lm3_list)
lm1fit$vcov <- lm_list_vcov(lm1_list)
lm2fit$vcov <- lm_list_vcov(lm2_list)
lm3fit$vcov <- lm_list_vcov(lm3_list)

lm_list_vcov <- function(object) {
    vcov0 <- lapply(object,
                    stats::vcov)
    est <- lm2ptable(object)$est
    ys <- sapply(object,
                 get_response)
    names(vcov0) <- ys
    est$uid <- seq_len(nrow(est))
    for (yy in ys) {
        vcov1 <- vcov0[[yy]]
        vcov_names <- colnames(vcov1)
        j <- est$lhs == yy
        i <- match(vcov_names, est[j, "rhs"])
        k <- est[j, "uid"][i]
        l <- match("(Intercept)", vcov_names)
        k[l] <- est[j & est$op == "~1", "uid"]
        m <- order(k)
        vcov2 <- vcov1[m, m]
        vcov0[[yy]] <- vcov2
      }
    vcov0
  }

cond_se <- function(xi,
                    est_vcov,
                    est) {
    if (all(is.na(xi))) return(0)
    if (is.null(xi$prod)) return(0)
    b_i <- xi$b
    w_i <- xi$w
    if (is.list(w_i)) {
        w_i0 <- sapply(w_i, paste0, collapse = ":")
      } else {
        w_i0 <- w_i
      }
    wvalues_i <- mapply(function(b1, w1, wvalues) {
                      prod(wvalues[w1])
                    },
                    b1 = b_i,
                    w1 = w_i,
                    MoreArgs = list(wvalues = wvalues))
    wv_na <- is.na(wvalues_i)
    if (isTRUE(any(wv_na))) {
        wvalues_i[wv_na] <- 0
        names(wvalues_i) <- w_i0
      }
    yi <- xi$y
    est_vcov_i <- est_vcov[[yi]][c(xi$x, w_i0), c(xi$x, w_i0), drop = FALSE]
    b0 <- matrix(c(1, wvalues_i),
                 ncol = 1)
    out <- t(b0) %*% est_vcov_i %*% b0
    out <- sqrt(as.numeric(out))
    out
  }

lm_list_vcov(lm1_list)

suppressMessages(suppressMessages(library(lavaan)))
dat$w1x <- dat$w1 * dat$x
dat$w1m <- dat$w1 * dat$m
dat$w2m <- dat$w2 * dat$m
dat$w1w2 <- dat$w1 * dat$w2
dat$w1mw2 <- dat$w1 * dat$m * dat$w2
mod2 <-
"
m ~ a * x + af1 * w1 + ad1 * w1x + c1 + c2
# Covariances not added. Not necessary for this test.
"
mod3 <-
"
y ~ b * m + cp * x + bf1 * w1 + bf2 * w2 +
    bd1 * w1m + bd2 * w2m + bd12 * w1w2 + be12 * w1mw2 + c1 + c2
# Covariances not added. Not necessary for this test.
"
fit2 <- sem(mod2, dat, fixed.x = FALSE, warn = FALSE)
fit3 <- sem(mod3, dat, fixed.x = FALSE, warn = FALSE)
est2 <- parameterEstimates(fit2)
est3 <- parameterEstimates(fit3)

wvalues <- c(w1 = 5, w2 = -4)

# Moderation
ce_1b <- indirect_i(x = "m", y = "y",
                    fit = NULL,
                    est = lm1fit$est,
                    data = lm1fit$data,
                    implied_stats = lm1fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = lm1fit$vcov)
ce_2b <- indirect_i(x = "x", y = "m",
                    fit = NULL,
                    est = lm2fit$est,
                    data = lm2fit$data,
                    implied_stats = lm2fit$implied_stats,
                    wvalues = wvalues,
                    est_vcov = )
ce_1b_chk2 <- (est[est$label == "a", "est"] +
                wvalues["w1"] * est[est$label == "ad1", "est"]) *
              (est[est$label == "b", "est"] +
                wvalues["w1"] * est[est$label == "bd1", "est"] +
                wvalues["w2"] * est[est$label == "bd2", "est"] +
                wvalues["w1"] * wvalues["w2"] * est[est$label == "be12", "est"])

ce_2b_chk <- indirect_i(x = "x", y = "y", m = "m",
                      est = lm2fit$est,
                      data = lm2fit$data,
                      wvalues = wvalues)

test_that("Check indirect: 3-way", {
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
