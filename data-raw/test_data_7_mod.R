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
