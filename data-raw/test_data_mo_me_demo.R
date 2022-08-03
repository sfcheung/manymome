
# Generate data

library(lavaan)
impute_missing <- function(data, mprob = .20) {
    n <- nrow(data)
    p <- ncol(data)
    m <- matrix(0, n, p)
    nstar <- n * p
    i <- sample(nstar, round(nstar * mprob))
    out <- data
    for (ii in i) {
        out[row(m)[ii], col(m)[ii]] <- NA
      }
    out
  }
set.seed(531535)
n <- 200
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
x1 <- ivs[, 1]
x2 <- ivs[, 2]
w1e <- ivs[, 3]
c1 <- ivs[, 4]
c2 <- ivs[, 5]
m1e <- rnorm(n)
m2e <- rnorm(n)
m3e <- rnorm(n)
y1e <- rnorm(n)
y2e <- rnorm(n)
w2e <- rnorm(n)

w1 <- .4 * x1 + sqrt(1 - .4^2) * w1e
m1 <- (.4 + .2 * w1) * x1 + .2 * x2 + sqrt(1 - .4^2 - .2^2) * m1e
m2 <- .5 * m1 + sqrt(1 - .5^2) * m2e
w2 <- .4 * m2 + sqrt(1 - .4^2) * w2e
y1 <- (.8 + .8 * w2) * m2 + .6 * (w2) + 1.8 * y1e

m3 <- .2 * x1 + .5 * x2 + sqrt(1 - .2^2 - .5^2) * m3e
y2 <- .6 * m3 + sqrt(1 - .6^2) * y2e

cor(w1, x1)
cor(w2, m2)

dat <- data.frame(x1, x2, m1, m2, m3, y1, y2, w1, w2, c1, c2)
head(dat)
dat0 <- dat
dat <- dat0
psych::describe(dat)
dat <- 1 * dat + 5
dat$x1 <- 1 * dat$x1
dat$x2 <- 1 * dat$x2
dat$w1 <- dat$w1 / 1
dat$w2 <- dat$w2 / 1
psych::describe(dat)

summary(lm_m1 <- lm(m1 ~ x1*w1 + x2 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ m1 + c1 + c2, dat))
summary(lm_y1 <- lm(y1 ~ m2*w2 + x1 + x2 + m3 + c1 + c2, dat))
summary(lm_m3 <- lm(m3 ~ x2 + x1 + c1 + c2, dat))
summary(lm_y2 <- lm(y2 ~ m3 + x2 + x1 + m2 + c1 + c2, dat))

data_mome_demo <- dat
set.seed(87351)
dat <- impute_missing(data_mome_demo, .015)
head(dat)
sum(complete.cases(dat))
data_mome_demo_missing <- dat

usethis::use_data(data_mome_demo, overwrite = TRUE)
usethis::use_data(data_mome_demo_missing, overwrite = TRUE)
