skip("WIP")

skip_on_cran()

library(testthat)
library(manymome)
suppressMessages(library(lavaan))

# Workflow: No implied statistics
# do_mc()
# --> fit2mc_out()
#   --> mc2est(): Convert mc estimates to a list of parameter tables
#     --> apply: set_est_i(): Create one parameter table

test_that("do_mc: Optimization", {

data(data_med_mod_ab1)
dat <- data_med_mod_ab1
mod <-
"
m ~ x + w + x:w + c1 + c2
y ~ m + w + x:w + x + c1 + c2
"
fit <- sem(mod, dat)

# pt1 <- ptable[, c("lhs", "op", "rhs", "est")]
# pt1 <- pt1[sample.int(nrow(pt1)), ]
# est_df$row_id <- seq_len(nrow(est_df))
# tmp <- merge(pt1, est_df, sort = FALSE)
# tmp2 <- est_df
# tmp2$est <- NA
# tmp2[tmp$row_id, "est"] <- pt1$est
# tmp2$row_id <- NULL
# est_df$row_id <- NULL
# tmp3 <- merge(est_df, ptable[, c("lhs", "op", "rhs", "est")],
#               sort = FALSE)
# identical(unclass(tmp2), unclass(tmp3))

system.time(
mc_out1 <- do_mc(fit, R = 100, seed = 1234)
)

system.time(
mc_out2 <- do_mc(fit, R = 100, seed = 1234, compute_implied_stats = FALSE)
)

ind1 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       mc_ci = TRUE,
                       mc_out = mc_out1)
ind1

ind2 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       mc_ci = TRUE,
                       mc_out = mc_out2)
ind2

expect_equal(confint(ind1),
             confint(ind2))

std1 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       mc_ci = TRUE,
                       mc_out = mc_out1,
                       standardized_x = TRUE,
                       standardized_y = TRUE)

std2 <- indirect_effect(x = "c1",
                       m = "m",
                       y = "y",
                       fit = fit,
                       mc_ci = TRUE,
                       mc_out = mc_out2,
                       standardized_x = TRUE,
                       standardized_y = TRUE)

expect_false(isFALSE(all.equal(confint(std1),
                               confint(std2))))

fit_implied <- lav_implied_all(fit)
x_sd <- sqrt(fit_implied$cov["c1", "c1"])
y_sd <- sqrt(fit_implied$cov["y", "y"])

expect_equal(confint(ind2) * x_sd / y_sd,
             confint(std2))

})
