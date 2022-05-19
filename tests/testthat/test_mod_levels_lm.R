
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
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

# mod_levels(fit, w = list(gp = c("gpgp2", "gpgp3"),
#                          w1 = "w1"))
# Output:
# gp:
# data.frame(label = c("cat1", "cat2"),
#            gpgp2 = c(1, 0),
#            gpgp3 = c(0, 1))
# w1:
# data.frame(label = c("Low", "Medium", "High"),
#            w1 = c(-2, 0, 2))
