
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

dat <- cbind(dat, factor2var(dat$gp, prefix = "gp", add_rownames = FALSE))
dat <- cbind(dat, factor2var(dat$city, prefix = "city", add_rownames = FALSE))

mod <-
"
m1 ~ x + w1 + x:w1
m2 ~ m1 + gpgp2 + gpgp3 + citybeta + citygamma + citysigma
m3 ~ m1 + x + gpgp2 + gpgp3 + x:gpgp2 + x:gpgp3
y ~ m2 + m3 + x + w4 + x:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)

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
