# Generate data
library(lavaan)
set.seed(5415413)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 5), diag(3))
x <- ivs[, 1]
c1 <- ivs[, 2]
c2 <- ivs[, 3]
m <- 10 + .9 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
y <- 5 + .9 * m + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, m, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m <- lm(m ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m + x + c1 + c2, dat))
mod <-
"
m ~ a * x + c1 + c2
y ~ b * m + x + c1 + c2
ab := a * b
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)
head(dat)
data_med <- dat
usethis::use_data(data_med, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(6543523)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 3, 2, 5), diag(4))
x <- ivs[, 1]
w <- ivs[, 2]
c1 <- ivs[, 3]
c2 <- ivs[, 4]
y <- 5 + .5 * x + .2 * w + .5 * w * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, w, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_y <- lm(y ~ x*w + c1 + c2, dat))
head(dat)
data_mod <- dat
usethis::use_data(data_mod, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(452634)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 4, 3, 2, 1), diag(5))
x <- ivs[, 1]
w1 <- ivs[, 2]
w2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
y <- 5 + 15.5 * x + .2 * w1 - 1.3 * w2 + 8.4 * w1 * x - 3.3 * w2 * x + .05 * c1 - .1 * c2 + rnorm(n, 0, 86)
dat <- data.frame(x, w1, w2, y, c1, c2)
dat$y <- dat$y / 100
head(dat)
colMeans(dat)
summary(lm_y <- lm(y ~ x*w1 + x*w2 + c1 + c2, dat))
head(dat)
data_mod2 <- dat
usethis::use_data(data_mod2, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(6541531)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 5, 2, 5), diag(5))
x <- ivs[, 1]
m1 <- ivs[, 2]
m2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
m1 <- 10 + .9 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
m2 <- 3 + .4 * m1 + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
y <- 5 + .5 * m2 + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, m1, m2, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m1 <- lm(m1 ~ x + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ m1 + x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m2 + m1 + x + c1 + c2, dat))
mod <-
"
m1 ~ a * x + c1 + c2
m2 ~ b1 * m1 + x + c1 + c2
y ~ b2 * m2 + m1 + x + c1 + c2
indirect := a * b1 * b2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)
head(dat)
data_serial <- dat
usethis::use_data(data_serial, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(2354652)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 5, 2, 5), diag(5))
x <- ivs[, 1]
m1 <- ivs[, 2]
m2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
m1 <- 10 + .9 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
m2 <- 3 + .4 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
y <- 5 + .5 * m2 + .4 * m1 + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, m1, m2, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m1 <- lm(m1 ~ x + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m2 + m1 + x + c1 + c2, dat))
mod <-
"
m1 ~ a1 * x + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b2 * m2 + b1 * m1 + x + c1 + c2
indirect1 := a1 * b1
indirect2 := a2 * b2
indirect := a1 * b1 + a2 * b2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)
head(dat)
data_parallel <- dat
usethis::use_data(data_parallel, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(753252)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 5, 3, 2, 5), diag(6))
x <- ivs[, 1]
m11 <- ivs[, 2]
m12 <- ivs[, 3]
m2 <- ivs[, 4]
c1 <- ivs[, 5]
c2 <- ivs[, 6]
m11 <- 10 + .9 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
m12 <- 10 + .5 * m11 + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
m2 <- 3 + .4 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
y <- 5 + .5 * m2 + .4 * m12 + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, m11, m12, m2, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m11 <- lm(m11 ~ x + c1 + c2, dat))
summary(lm_m12 <- lm(m12 ~ m11 + x + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m2 + m11 + m12 + x + c1 + c2, dat))
mod <-
"
m11 ~ a11 * x + c1 + c2
m12 ~ b11 * m11 + x + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b12 * m12 + b2 * m2 + m11 + x + c1 + c2
indirect1 := a11 * b11 * b12
indirect2 := a2 * b2
indirect := a11 * b11 * b12 + a2 * b2
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)
head(dat)
data_serial_parallel <- dat
usethis::use_data(data_serial_parallel, overwrite = TRUE)
