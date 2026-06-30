# Generate data
library(lavaan)
set.seed(64324132)
e_sd <- sqrt(1 - .2^2)
# gp: "Control"; : site: "Site 1"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, 1)
y <- .0 * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
dat1 <- data.frame(x, y, c1, c2)
dat1$gp <- "Control"
dat1$site <- "Site 1"
# gp: "Treatment"; site: "Site 1"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, 1)
y <- -(-1 * .4) + .4 * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
dat2 <- data.frame(x, y, c1, c2)
dat2$gp <- "Treatment"
dat2$site <- "Site 1"
# gp: "Control"; : site: "Site 2"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, 1)
y <- .0 * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
dat3 <- data.frame(x, y, c1, c2)
dat3$gp <- "Control"
dat3$site <- "Site 2"
# gp: "Treatment"; site: "Site 2"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, 1)
y <- -(-1 * .9) + .9 * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
dat4 <- data.frame(x, y, c1, c2)
dat4$gp <- "Treatment"
dat4$site <- "Site 2"
# gp: "Control"; : site: "Site 3"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, 1)
y <- .0 * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
dat5 <- data.frame(x, y, c1, c2)
dat5$gp <- "Control"
dat5$site <- "Site 3"
# gp: "Treatment"; site: "Site 3"
n <- 100
c1 <- rnorm(n)
c2 <- rnorm(n)
x <- rnorm(n, 0, 1)
y <- .0 * x + rnorm(n, 0, e_sd) + .1 * c2 + .2 * c2
dat6 <- data.frame(x, y, c1, c2)
dat6$gp <- "Treatment"
dat6$site <- "Site 3"

dat <- do.call(
  rbind,
  list(dat1,
       dat2,
       dat3,
       dat4,
       dat5,
       dat6)
)

lm_out1 <- lm(y ~ x*gp + c1 + c2, dat)
summary(lm_out1)
library(manymome)
out1 <- cond_effects(
  wlevels = "gp",
  x = "x",
  fit = lm_out1,
)
out1
plot(out1, graph_type = "tumble")

lm_out2 <- lm(y ~ x*gp + x*site + c1 + c2, dat)
summary(lm_out2)
library(manymome)
out2 <- cond_effects(
  wlevels = c("site", "gp"),
  x = "x",
  fit = lm_out2,
)
out2
plot(out2, graph_type = "tumble")
plot(out2, facet_grid_cols = "gp", graph_type = "tumble")
plot(out2, facet_grid_cols = "site", graph_type = "tumble")

lm_out3 <- lm(y ~ x*gp*site + c1 + c2, dat)
summary(lm_out3)
library(car)
Anova(lm_out3)
Anova(lm_out3, type = 3)
library(manymome)
out3 <- cond_effects(
  wlevels = c("site", "gp"),
  x = "x",
  fit = lm_out3,
)
out3
plot(out3, graph_type = "tumble")
plot(out3, facet_grid_cols = "gp", graph_type = "tumble")
plot(out3, facet_grid_cols = "site", graph_type = "tumble")
plot(out3, facet_grid_cols = "site")

psych::describe(dat)

dat0 <- dat
dat <- dat[, c("x", "y", "c1", "c2")]
dat <- scale(dat, center = -c(5, 6, 7, 4) + rnorm(4, 0, .2), scale = FALSE)
dat <- scale(dat, center = FALSE, scale = 1 / runif(4, 3, 5))
dat <- as.data.frame(round(dat, 2))
dat$gp <- dat0$gp
dat$site <- dat0$site
psych::describe(dat)

lm_out1 <- lm(y ~ x*gp + c1 + c2, dat)
summary(lm_out1)
lm_out2 <- lm(y ~ x*gp + x*site + c1 + c2, dat)
summary(lm_out2)
lm_out3 <- lm(y ~ x*gp*site + c1 + c2, dat)
summary(lm_out3)

data_mod_cat_2w <- dat
# usethis::use_data(data_mod_cat_2w, overwrite = TRUE)

