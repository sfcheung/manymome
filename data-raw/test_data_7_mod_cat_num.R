# Generate data
library(lavaan)
set.seed(4145423)
e_sd <- sqrt(1 - .2^2)
# city: "City A"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, .5)
w <- rnorm(n)
y <- .0 * x + rnorm(n, 0,  e_sd) + .1 * c2 + .2 * c2
dat1 <- data.frame(x, w, y, c1, c2)
dat1$city <- "City A"
# city: "City B"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, .5, 1)
w <- rnorm(n)
y <- -(-1 * .6) + (.6 + .6*w) * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
y <- y + .5 * w
dat2 <- data.frame(x, w, y, c1, c2)
dat2$city <- "City B"

dat <- do.call(
  rbind,
  list(dat1,
       dat2)
)

lm_out1 <- lm(y ~ x*w*city + c1 + c2, dat)
summary(lm_out1)
library(manymome)
out1 <- cond_effects(
  wlevels = c("city", "w"),
  x = "x",
  fit = lm_out1,
)
out1
plot(out1, facet_grid_cols = "city", graph_type = "tumble")

lm_out2 <- lm(y ~ x*w + x*city + c1 + c2, dat)
summary(lm_out2)
library(manymome)
out2 <- cond_effects(
  wlevels = c("city", "w"),
  x = "x",
  fit = lm_out2,
)
out2
plot(out2, facet_grid_cols = "city", graph_type = "tumble")

lm_out3 <- lm(y ~ x*w + c1 + c2, dat)
summary(lm_out3)
library(manymome)
out3 <- cond_effects(
  wlevels = c("w"),
  x = "x",
  fit = lm_out3,
)
out3
plot(out3, graph_type = "tumble")

psych::describe(dat)

dat0 <- dat
dat <- dat[, c("x", "w", "y", "c1", "c2")]
dat <- scale(dat, center = -c(5, 4, 6, 7, 4) + rnorm(5, 0, .2), scale = FALSE)
dat <- scale(dat, center = FALSE, scale = 1 / runif(5, 3, 5))
dat <- as.data.frame(round(dat, 2))
dat$city <- dat0$city
psych::describe(dat)

lm_out1 <- lm(y ~ x*w + c1 + c2, dat)
summary(lm_out1)
lm_out2 <- lm(y ~ x*city + x*w + c1 + c2, dat)
summary(lm_out2)
lm_out3 <- lm(y ~ x*w*city + c1 + c2, dat)
summary(lm_out3)

data_mod_cat_num_2w <- dat
usethis::use_data(data_mod_cat_num_2w, overwrite = TRUE)

