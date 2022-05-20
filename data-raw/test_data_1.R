# Generate data
library(lavaan)
set.seed(80511)
n <- 200
ivs <- MASS::mvrnorm(n, c(.2, .3, .4, .2, .1), diag(5))
x <- ivs[, 1]
w1 <- ivs[, 2]
w2 <- ivs[, 3]
w3 <- ivs[, 4]
w4 <- ivs[, 5]
m1 <- 10 * 1 + .4 * x + .2 * w1 + .3 * x * w1 + rnorm(n, 0, 1)
m2 <- 5 * 1 + .5 * m1 + .2 * w2 + .1 * m1 * w2 + rnorm(n, 0, 1.5)
m3 <- 6 * 1 + .6 * m2 + .3 * w3 + .2 * m2 * w3 + rnorm(n, 0, 3)
y <- 8 * 1 + .4 * m3 + .3 * w4 + .2 * m3 * w4 + .3 * x + rnorm(n, 0, 3)
dat <- data.frame(x, w1, w2, w3, w4, m1, m2, m3, y)
head(dat)
colMeans(dat)
summary(lm_m1 <- lm(m1 ~ x * w1, dat))
summary(lm_m2 <- lm(m2 ~ m1 * w2, dat))
summary(lm_m3 <- lm(m3 ~ m2 * w3, dat))
summary(lm_y <- lm(y ~ m3 * w4 + x, dat))

summary(lm_m1 <- lm(m1 ~ x, dat))
summary(lm_m2 <- lm(m2 ~ m1, dat))
summary(lm_m3 <- lm(m3 ~ m2, dat))
summary(lm_y <- lm(y ~ m3 + x, dat))

mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
fit
head(dat)
dat$gp <- sample(c("blue", "yellow", "green"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
modmed_x1m3w4y1 <- dat
usethis::use_data(modmed_x1m3w4y1, overwrite = TRUE)

