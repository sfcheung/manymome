# Generate data
library(lavaan)
set.seed(532432)
n <- 200
w1 <- rnorm(n)
w2 <- rnorm(n)
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- 5 * (w1 + 5) * rnorm(n)
x <- scale(x)[, 1]
x <- .6 * w1 + x
plot(w1, x)
y <- (.3 + .6 * w1) * x + (.0 + -.2 * w2) * x + .2 * c1 + .3 * c2 + rnorm(n, 0, 1.5)
y <- .3 + w1 + -.4 * w2 + y
lm_out <- lm(y ~ x*w1 + x*w2 + c1 + c2)
summary(lm_out)
library(manymome)
out <- cond_effects(
  wlevels = c("w1", "w2"),
  x = "x",
  fit = lm_out
)
out
plot(out, facet_grid_cols = "w1", graph_type = "tumble")
plot(out, facet_grid_cols = "w2", graph_type = "tumble")
plot(out, facet_grid_cols = "w1")
plot(out, facet_grid_cols = "w2")
dat <- data.frame(y, x, w1, w2, c1, c2)
dat <- scale(dat)
dat <- scale(dat, center = -c(5, 6, 7, 4, 6, 5) + rnorm(6, 0, .2), scale = FALSE)
dat <- scale(dat, center = FALSE, scale = runif(6, .8, 1.2))
dat <- as.data.frame(round(dat, 2))
psych::describe(dat)
lm_out <- lm(y ~ x*w1 + x*w2 + c1 + c2)
summary(lm_out)
library(manymome)
out <- cond_effects(
  wlevels = c("w1", "w2"),
  x = "x",
  fit = lm_out
)
out
data_mod_2w <- dat
usethis::use_data(data_mod_2w, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(67442)
n <- 200
w1 <- rnorm(n)
c1 <- rnorm(n)
c2 <- rnorm(n)
x1 <- rnorm(n)
x2 <- rnorm(n) / (w1 + 5)
x2 <- scale(x2)[, 1]
x2 <- .6 * w1 + x2
plot(w1, x1)
plot(w1, x2)
y <- (.1 + .6 * w1) * x2 + (.2 + .6 * w1) * x1 + .2 * c1 + .3 * c2 + rnorm(n, 0, 1.5)
y <- .3 + w1 + y
lm_out <- lm(y ~ x1*w1 + x2*w1 + c1 + c2)
summary(lm_out)
library(manymome)
out1 <- cond_effects(
  wlevels = "w1",
  x = "x1",
  fit = lm_out
)
out1
out2 <- cond_effects(
  wlevels = "w1",
  x = "x2",
  fit = lm_out
)
out2
plot(out1, graph_type = "tumble")
plot(out2, graph_type = "tumble")
dat <- data.frame(y, x1, x2, w = w1, c1, c2)
dat <- scale(dat)
dat <- scale(dat, center = -c(5, 6, 7, 4, 6, 5) + rnorm(6, 0, .2), scale = FALSE)
dat <- scale(dat, center = FALSE, scale = runif(6, 2, 5))
dat <- as.data.frame(round(dat, 2))
psych::describe(dat)
lm_out <- lm(y ~ x1*w + x2*w + c1 + c2, dat)
summary(lm_out)
library(manymome)
out1 <- cond_effects(
  wlevels = "w",
  x = "x1",
  fit = lm_out
)
out1
out2 <- cond_effects(
  wlevels = "w",
  x = "x2",
  fit = lm_out
)
out2
data_mod_2x1w <- dat
usethis::use_data(data_mod_2x1w, overwrite = TRUE)

# Generate data
library(lavaan)
set.seed(5614543)
n <- 200
w1 <- rnorm(n)
w2 <- rnorm(n)
c1 <- rnorm(n)
c2 <- rnorm(n)
x1 <- 5 * (w1 + 5) * rnorm(n)
x1 <- x1 + 2 * w1
x1 <- scale(x)[, 1]
x2 <- rnorm(n)
cor(x1, w1)
cor(x2, w2)
plot(w1, x1)
y <- (.4 + .6 * w1) * x1 + (5 + 5 * w2) * x2 + .2 * c1 + .3 * c2 + rnorm(n, 0, 25)
y <- 12 * w1 + 5 * w2 + y
lm_out <- lm(y ~ x1*w1 + x2*w2 + c1 + c2)
summary(lm_out)

library(manymome)
out1 <- cond_effects(
  wlevels = "w1",
  x = "x1",
  fit = lm_out
)
out1
out2 <- cond_effects(
  wlevels = "w2",
  x = "x2",
  fit = lm_out
)
out2

plot(out1, graph_type = "tumble")
plot(out2, graph_type = "tumble")
dat <- data.frame(y, x1, x2, w1, w2, c1, c2)
dat <- scale(dat)
dat <- scale(dat, center = -c(5, 6, 7, 8, 4, 6, 5) + rnorm(7, 0, .2), scale = FALSE)
dat <- scale(dat, center = FALSE, scale = runif(7, 2, 10))
dat <- as.data.frame(round(dat, 2))
psych::describe(dat)
lm_out <- lm(y ~ x1*w1 + x2*w2 + c1 + c2)
summary(lm_out)
library(manymome)
out1 <- cond_effects(
  wlevels = "w1",
  x = "x1",
  fit = lm_out
)
out1
out2 <- cond_effects(
  wlevels = "w2",
  x = "x2",
  fit = lm_out
)
out2
data_mod_2x2w <- dat
usethis::use_data(data_mod_2x2w, overwrite = TRUE)
