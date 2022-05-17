
library(stdmodsem)
library(lavaan)
dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 6:7]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)
mod <-
paste(
    "m1 ~", paste(names(coef(lm_m1))[-1], collapse = " + "), "\n",
    "m2 ~", paste(names(coef(lm_m2))[-1], collapse = " + "), "\n",
    "m3 ~", paste(names(coef(lm_m3))[-1], collapse = " + "), "\n",
    "y ~", paste(names(coef(lm_y))[-1], collapse = " + "), "\n"
  )
# cat(mod)
fit <- sem(mod, dat2, meanstructure = TRUE,
           se = "none", baseline = FALSE, h1 = FALSE,
           likelihood = "wishart")
ptable <- parameterTable(fit)
p_path <- ptable[(ptable$op == "~") | (ptable$op == "~1"),
                 c("lhs", "op", "rhs", "est")]
out <- lm2ptable(list(lm_m1, lm_m2, lm_m3, lm_y))

chk <- merge(p_path, out$est, by = c("lhs", "op", "rhs"))

test_that("lm2ptable", {
    expect_true(all.equal(chk$est.x, chk$est.y, tolerance = .0001))
  })
