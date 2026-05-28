skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

# Test: Simple mediation

test_that("q function: simple mediation: sem: std with fixed.x FALSE", {

set.seed(1234)
data_med$city <- sample(c("city1", "city2"), nrow(data_med), replace = TRUE)

data_med[1:5, "c2"] <- NA

out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1", "city"),
                          data = data_med,
                          ci_type = "mc",
                          fit_method = "sem",
                          fixed.x = FALSE,
                          parallel = FALSE,
                          progress = FALSE)

data_med$city0 <- as.numeric(as.factor(data_med$city))
mod <-
"
m ~ x + c2 + c1 + city0
y ~ m + x + c2 + c1 + city0
"
fit <- sem(
          mod,
          data = data_med,
          missing = "fiml",
          fixed.x = FALSE
        )
# parameterEstimates(fit, standardized = "std.nox")
std <- standardizedSolution(fit, type = "std.nox")

out1 <- lm_from_lavaan_list_for_q(
                    fit = out$lm_out,
                    mm = out$model_matrices,
                    ci_level = .95,
                    rsq_test = FALSE
                  )
out1$y$coefs_lm["citycity2", "betaS"]

expect_equal(std[(std$lhs == "y") & (std$rhs == "city0"), "est.std"],
             out1$y$coefs_lm["citycity2", "betaS"],
             tolerance = 1e-5)

})
