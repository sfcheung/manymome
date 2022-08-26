# Generate data
library(lavaan)
set.seed(64523)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 4, 5), diag(4))
x <- ivs[, 1]
w <- ivs[, 2]
c1 <- ivs[, 3]
c2 <- ivs[, 4]
m <- 10 + .9 * x + .2 * w + .6 * x * w + .2 * c1 - .2 * c2 + rnorm(n, 0, 4)
y <- 5 + .9 * m + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 4)
dat <- data.frame(x, w, m, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m <- lm(m ~ x*w + c1 + c2, dat))
summary(lm_y <- lm(y ~ m + x + w + c1 + c2, dat))
mod <-
"
m ~ a * x + w + d * x:w + c1 + c2
y ~ b * m + x + w + c1 + c2
w ~~ v_m * w
w ~ m_w * 1
ab := a * b
ab_lo := (a + d * (m_w - sqrt(v_m))) * b
ab_hi := (a + d * (m_w + sqrt(v_m))) * b
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 31, 32, 33), ]
head(dat)
data_med_mod_a <- dat
usethis::use_data(data_med_mod_a, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(64523)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 4, 5), diag(4))
m <- ivs[, 1]
w <- ivs[, 2]
c1 <- ivs[, 3]
c2 <- ivs[, 4]
y <- 10 + .9 * m + .2 * w + .6 * m * w + .2 * c1 - .2 * c2 + rnorm(n, 0, 4)
x <- 5 + .5 * m + rnorm(n, 0, 1)
dat <- data.frame(x, w, m, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m <- lm(m ~ x + w + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w + x + c1 + c2, dat))
mod <-
"
m ~ a * x + w + c1 + c2
y ~ b * m + x + d * m:w + c1 + c2
w ~~ v_m * w
w ~ m_w * 1
ab := a * b
ab_lo := a * (b + d * (m_w - sqrt(v_m)))
ab_hi := a * (b + d * (m_w + sqrt(v_m)))
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 7, 30, 31, 32), ]
head(dat)
data_med_mod_b <- dat
usethis::use_data(data_med_mod_b, overwrite = TRUE)


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
w1 <- .7 * x + rnorm(n, 0, sqrt(1 - .7^2))
cor(w1, x)
m <- .1 * x + .0 * w1 + .25 * x * w1 + rnorm(n, 0, .9)
w2 <- .8 * scale(m)[, 1] + rnorm(n, 0, sqrt(1 - .8^2))
cor(w2, m)
y <- .3 * m + .35 * m * w2 + rnorm(n, 0, .8)
dat <- data.frame(x, w1, w2, m, y, c1, c2)
head(dat)
summary(lm_m <- lm(m ~ x*w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w2 + x + w1 + c1 + c2, dat))
dat <- as.data.frame(scale(dat, center = c(-10, -5, -3, -4, -8, -10, -5), scale = FALSE))
colMeans(dat)
summary(lm_m <- lm(m ~ x*w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w2 + x + w1 + c1 + c2, dat))

mod <-
"
m ~ a * x + w1 + d1 * x:w1 + c1 + c2
y ~ b * m + x + w1 + w2 + d2 * m:w2 + c1 + c2
w1 ~~ v_m1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_m2 * w2
w2 ~ m_w2 * 1
ab := a * b
ab_lolo := (a + d1 * (m_w1 - sqrt(v_m1))) * (b + d2 * (m_w2 - sqrt(v_m2)))
ab_lohi := (a + d1 * (m_w1 - sqrt(v_m1))) * (b + d2 * (m_w2 + sqrt(v_m2)))
ab_hilo := (a + d1 * (m_w1 + sqrt(v_m1))) * (b + d2 * (m_w2 - sqrt(v_m2)))
ab_hihi := (a + d1 * (m_w1 + sqrt(v_m1))) * (b + d2 * (m_w2 + sqrt(v_m2)))
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 10, 41:45), ]
head(dat)
data_med_mod_ab <- dat
usethis::use_data(data_med_mod_ab, overwrite = TRUE)


# Generate data
library(lavaan)
set.seed(553553)
n <- 100
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0), diag(4))
x <- ivs[, 1]
w <- ivs[, 2]
c1 <- ivs[, 3]
c2 <- ivs[, 4]
w <- .7 * x + rnorm(n, 0, sqrt(1 - .7^2))
cor(w, x)
m <- .1 * x + .2 * w + .35 * x * w + rnorm(n, 0, .9)
cor(w, m)
y <- .3 * m + .35 * m * w + rnorm(n, 0, .8)
dat <- data.frame(x, w, m, y, c1, c2)
head(dat)
summary(lm_m <- lm(m ~ x*w + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w + x + c1 + c2, dat))
dat <- as.data.frame(scale(dat, center = c(-10, -5, -3, -4, -8, -10), scale = FALSE))
colMeans(dat)
summary(lm_m <- lm(m ~ x*w + c1 + c2, dat))
summary(lm_y <- lm(y ~ m*w + x + c1 + c2, dat))

mod <-
"
m ~ a * x + w + da * x:w + c1 + c2
y ~ b * m + x + w + db * m:w + c1 + c2
w ~~ v_m * w
w ~ m_w * 1
ab := a * b
ab_lo := (a + da * (m_w - sqrt(v_m))) * (b + db * (m_w - sqrt(v_m)))
ab_hi := (a + da * (m_w + sqrt(v_m))) * (b + db * (m_w + sqrt(v_m)))
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 9, 38:40), ]
head(dat)
data_med_mod_ab1 <- dat
usethis::use_data(data_med_mod_ab1, overwrite = TRUE)



# Generate data
library(lavaan)
set.seed(553553)
n <- 100
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
x <- ivs[, 1]
w1 <- ivs[, 2]
w2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
w1 <- .7 * x + rnorm(n, 0, sqrt(1 - .7^2))
cor(w1, x)
m1 <- .1 * x + .0 * w1 + .25 * x * w1 + rnorm(n, 0, .9)
m2 <- .3 * x + rnorm(n, 0, .9)
w2 <- .8 * scale(m2)[, 1] + rnorm(n, 0, sqrt(1 - .8^2))
cor(w2, m2)
y <- .3 * m1 + .4 * m2 + .35 * m2 * w2 + rnorm(n, 0, .8)
dat <- data.frame(x, w1, w2, m1, m2, y, c1, c2)
head(dat)
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x*w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m1 + m2*w2 + x + w1 + c1 + c2, dat))
dat <- as.data.frame(scale(dat, center = c(-10, -5, -3, -4, -8, -5, -10, -5), scale = FALSE))
colMeans(dat)
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x*w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m1 + m2*w2 + x + w1 + c1 + c2, dat))

mod <-
"
m1 ~ a1 * x + w1 + da1 * x:w1 + c1 + c2
m2 ~ a2 * x + w1 + c1 + c2
y ~ b1 * m1 + b2 * m2 + x + w1 + w2 + db2 * m2:w2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
a1b1 := a1 * b1
a2b2 := a2 * b2
a1b1_w1lo := (a1 + da1 * (m_w1 - sqrt(v_w1))) * b1
a1b1_w1hi := (a1 + da1 * (m_w1 + sqrt(v_w1))) * b2
a2b2_w2lo := a2 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
a2b2_w2hi := a2 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 10, 11, 15, 48:53), ]
head(dat)
data_med_mod_parallel <- dat
usethis::use_data(data_med_mod_parallel, overwrite = TRUE)


# Generate data
library(lavaan)
set.seed(553553)
n <- 100
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
x <- ivs[, 1]
w1 <- ivs[, 2]
w2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
w1 <- .7 * x + rnorm(n, 0, sqrt(1 - .7^2))
cor(w1, x)
m1 <- .1 * x + .0 * w1 + .25 * x * w1 + rnorm(n, 0, .9)
m2 <- .3 * m1 + rnorm(n, 0, .9)
w2 <- .8 * scale(m2)[, 1] + rnorm(n, 0, sqrt(1 - .8^2))
cor(w2, m2)
y <- .4 * m2 + .35 * m2 * w2 + rnorm(n, 0, .8)
dat <- data.frame(x, w1, w2, m1, m2, y, c1, c2)
head(dat)
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ m1 + x + w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m2*w2 + m1 + x + w1 + c1 + c2, dat))
dat <- as.data.frame(scale(dat, center = c(-10, -5, -3, -4, -8, -5, -10, -5), scale = FALSE))
colMeans(dat)
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ m1 + x + w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m2*w2 + m1 + x + w1 + c1 + c2, dat))

mod <-
"
m1 ~ a * x + w1 + da1 * x:w1 + c1 + c2
m2 ~ b1 * m1 + x + w1 + c1 + c2
y ~ b2 * m2 + m1 + x + w1 + w2 + db2 * m2:w2 + c1 + c2
w1 ~~ v_m1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_m2 * w2
w2 ~ m_w2 * 1
ab1b2 := a * b1 * b2
ab1b2_lolo := (a + da1 * (m_w1 - sqrt(v_m1))) * b1 * (b2 + db2 * (m_w2 - sqrt(v_m2)))
ab1b2_lohi := (a + da1 * (m_w1 - sqrt(v_m1))) * b1 * (b2 + db2 * (m_w2 + sqrt(v_m2)))
ab1b2_hilo := (a + da1 * (m_w1 + sqrt(v_m1))) * b1 * (b2 + db2 * (m_w2 - sqrt(v_m2)))
ab1b2_hihi := (a + da1 * (m_w1 + sqrt(v_m1))) * b1 * (b2 + db2 * (m_w2 + sqrt(v_m2)))
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[c(1, 3, 6, 11, 16, 49:53), ]
head(dat)
data_med_mod_serial <- dat
usethis::use_data(data_med_mod_serial, overwrite = TRUE)



# Generate data
library(lavaan)
set.seed(553553)
n <- 100
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
x <- ivs[, 1]
w1 <- ivs[, 2]
w2 <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
w1 <- .7 * x + rnorm(n, 0, sqrt(1 - .7^2))
cor(w1, x)
m11 <- .1 * x + .0 * w1 + .25 * x * w1 + rnorm(n, 0, .9)
m12 <- .3 * m11 + rnorm(n, 0, .4)
m2 <- .3 * x + rnorm(n, 0, .4)
w2 <- .8 * scale(m2)[, 1] + rnorm(n, 0, sqrt(1 - .8^2))
cor(w2, m2)
y <- .3 * m11 + .4 * m2 + .35 * m2 * w2 + rnorm(n, 0, .8)
dat <- data.frame(x, w1, w2, m11, m12, m2, y, c1, c2)
head(dat)
summary(lm_m11 <- lm(m11 ~ x*w1 + c1 + c2, dat))
summary(lm_m12 <- lm(m12 ~ m11 + x + w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x + w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m11 + m12 + m2*w2 + x + w1 + c1 + c2, dat))
dat <- as.data.frame(scale(dat, center = c(-10, -5, -3, -4, -8, -5, -10, -5, -5), scale = FALSE))
colMeans(dat)
summary(lm_m11 <- lm(m11 ~ x*w1 + c1 + c2, dat))
summary(lm_m12 <- lm(m12 ~ m11 + x + w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x + w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m11 + m12 + m2*w2 + x + w1 + c1 + c2, dat))
mod <-
"
m11 ~ a1 * x + w1 + da11 * x:w1 + c1 + c2
m12 ~ b11 * m11 + x + w1 + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b12 * m12 + b2 * m2 + m11 + x + w1 + w2 + db2 * m2:w2 + c1 + c2
w1 ~~ v_w1 * w1
w1 ~ m_w1 * 1
w2 ~~ v_w2 * w2
w2 ~ m_w2 * 1
a1b11b22 := a1 * b11 * b12
a2b2 := a2 * b2
ab := a1b11b22 + a2b2
a1b11b12_w1lo := (a1 + da11 * (m_w1 - sqrt(v_w1))) * b11 * b12
a1b11b12_w1hi := (a1 + da11 * (m_w1 + sqrt(v_w1))) * b11 * b12
a2b2_w2lo := a2 * (b2 + db2 * (m_w2 - sqrt(v_w2)))
a2b2_w2hi := a2 * (b2 + db2 * (m_w2 + sqrt(v_w2)))
"
dat$w1 <- dat$w1 / 10
dat$w2 <- dat$w2 / 10
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE)
parameterEstimates(fit)[parameterEstimates(fit)$label != "", ]
data_med_mod_serial_parallel <- dat
usethis::use_data(data_med_mod_serial_parallel, overwrite = TRUE)
