skip_on_cran()

skip("WIP")

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem: betaS", {
dat <- data_med
head(dat)
set.seed(2345)
dat$c1 <- sample.int(2, nrow(dat), replace = TRUE)
# dat$c1 <- sample(c("gp1", "gp2", "gp3"),
#                  nrow(dat),
#                  replace = TRUE)
suppressWarnings(
out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 50,
                          seed = 1234,
                          fit_method = "sem",
                          parallel = FALSE,
                          progress = !is_testing())
)
suppressWarnings(
out1 <- q_simple_mediation(x = "x",
                           y = "y",
                           m = "m",
                           cov = list(y = c("c2", "c1"),
                                      m = "c2"),
                           data = data_med,
                           R = 50,
                           seed = 1234,
                           fit_method = "sem",
                           boot_type = "bc",
                           parallel = FALSE,
                           fixed.x = FALSE,
                           progress = !is_testing())
)
mod <-
"
m ~ x + c2 + c1
y ~ m + x + c2 + c1
"
mod1 <-
"
m ~ x + c2
y ~ m + x + c2 + c1
"
fit <- sem(
          mod,
          data = data_med,
          fixed.x = TRUE
        )
fit1 <- sem(
          mod1,
          data = data_med,
          fixed.x = FALSE
        )

est <- parameterEstimates(fit, standardized = TRUE)
i <- (est$lhs == "y") &
     (est$op == "~") &
     (est$rhs != "1")
chk <- est[i, c("rhs", "est", "std.all", "std.nox")]
exp <- out$lm_out_lav$y$coefs_lm[chk$rhs, ]

# If fixed.x = TRUE, betaS == std.all
expect_equal(
  exp[, "betaS"],
  chk$std.all,
  ignore_attr = TRUE
)

est <- parameterEstimates(fit1, standardized = TRUE)
i <- (est$lhs == "y") &
     (est$op == "~") &
     (est$rhs != "1")
chk <- est[i, c("rhs", "est", "std.all")]
exp <- out1$lm_out_lav$y$coefs_lm[chk$rhs, ]

# If fixed.x = FALSE, betaS == std.all
expect_equal(
  exp[, "betaS"],
  chk$std.all,
  ignore_attr = TRUE,
  tolerance = 1e-5
)

})
