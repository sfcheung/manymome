options(width = 132)

# Generate data
library(lavaan)
set.seed(235452)
n <- 100
ivs <- MASS::mvrnorm(n, rep(0, 6), diag(6))
x1 <- ivs[, 1]
x2 <- ivs[, 2]
c1 <- ivs[, 3]
c2 <- ivs[, 4]
em11 <- rnorm(n, 0, 1)
em12 <- rnorm(n, 0, 1)
em2 <- rnorm(n, 0, 1)
ey1 <- rnorm(n, 0, 1)
ey2 <- rnorm(n, 0, 1)
m11 <- .4 * x1 + .1 * c1 - .2 * c2 + 1 * em11
m12 <- .4 * m11 + .1 * c1 - .2 * c2 + 1 * em12
m2 <- .4 * x2 + .1 * c1 - .2 * c2 + 1 *em2
y1 <- .4 * m12 + (-.4) * m2 + .0 * x1 + .0 * x2 +  1 * ey1
y2 <- .0 * m12 + (-.4) * m2 + .0 * x1 + .0 * x2 +  1 * ey2
dat <- data.frame(x1, x2, m11, m12, m2, y1, y2, c1, c2)
head(dat)
psych::describe(dat)
dat <- as.data.frame(scale(dat,
        center = c(-10, -5, -5, -5, -8, -5, -10, -5, -10), scale = FALSE))
psych::describe(dat)
summary(lm_m11 <- lm(m11 ~ x1 + x1 + x2 + c1 + c2, dat))
summary(lm_m12 <- lm(m12 ~ m11 + x1 + x2 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x1 + x2 + c1 + c2, dat))
summary(lm_y1 <- lm(y1 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat))
summary(lm_y2 <- lm(y2 ~ m11 + m12 + m2 + x1 + x2 + c1 + c2, dat))
head(dat)
data_med_complicated <- dat
usethis::use_data(data_med_complicated, overwrite = TRUE)
