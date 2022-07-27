
# Generate data
library(lavaan)
set.seed(55355353)
n <- 100
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
x <- ivs[, 1]
w1 <- ivs[, 2]
w2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
cor(w1, x)
m <- .4 * x + rnorm(n, 0, .9)
w1 <- .5 * scale(m)[, 1] + rnorm(n, 0, sqrt(1 - .5^2))
w2 <- .5 * scale(m)[, 1] + rnorm(n, 0, sqrt(1 - .5^2))
cor(w1, m)
cor(w2, m)
y <- .3 * m + .35 * m * w1 * w2 +  rnorm(n, 0, .8)
dat <- data.frame(x, w1, w2, m, y, c1, c2)
head(dat)
summary(lm_m <- lm(m ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w1*w2 + x + c1 + c2, dat))
psych::describe(dat)
dat <- as.data.frame(scale(dat, center = c(-10, -5, -3, -4, -8, -10, -5), scale = FALSE))
colMeans(dat)
summary(lm_m <- lm(m ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w1*w2 + x + c1 + c2, dat))
data_med_mod_b_mod <- dat
usethis::use_data(data_med_mod_b_mod, overwrite = TRUE)

