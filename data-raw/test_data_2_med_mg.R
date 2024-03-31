# Generate data
library(lavaan)
set.seed(3143214)
n <- 100
ivs <- MASS::mvrnorm(n, c(10, 2, 5), diag(3))
x <- ivs[, 1]
c1 <- ivs[, 2]
c2 <- ivs[, 3]
m <- 10 + .9 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
y <- 5 + .6 * m + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, m, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m <- lm(m ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m + x + c1 + c2, dat))
dat1 <- dat
n <- 150
ivs <- MASS::mvrnorm(n, c(10, 2, 5), diag(3))
x <- ivs[, 1]
c1 <- ivs[, 2]
c2 <- ivs[, 3]
m <- 10 + .4 * x + .2 * c1 - .2 * c2 + rnorm(n, 0, 1)
y <- 5 + .9 * m + .2 * x + .1 * c1 - .1 * c2 + rnorm(n, 0, 2)
dat <- data.frame(x, m, y, c1, c2)
head(dat)
colMeans(dat)
summary(lm_m <- lm(m ~ x + c1 + c2, dat))
summary(lm_y <- lm(y ~ m + x + c1 + c2, dat))
dat2 <- dat
dat1$group <- "Group A"
dat2$group <- "Group B"
dat <- rbind(dat1, dat2)
mod <-
"
m ~ c(a1, a2) * x + c1 + c2
y ~ c(b1, b2) * m + x + c1 + c2
a1b1 := a1 * b1
a2b2 := a2 * b2
abdiff := a2b2 - a1b1
adiff := a2 - a1
bdiff := b2 - b1
"
fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE,
           group = "group")
parameterEstimates(fit)[41:45, ]
head(dat)
data_med_mg <- dat
usethis::use_data(data_med_mg, overwrite = TRUE)

