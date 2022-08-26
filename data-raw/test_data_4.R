# Generate data
library(lavaan)
set.seed(65235452)
n <- 300
ivs <- MASS::mvrnorm(n, c(10, 3, 2, 5), diag(4))
c1 <- ivs[, 3]
c2 <- ivs[, 4]
w <- sample(c("group1", "group2", "group3"), n, replace = TRUE)
x <- sapply(w, function(x) {switch(x,
                            group1 = rnorm(1, 1),
                            group2 = rnorm(1, 1.2),
                            group3 = rnorm(1, 1.3))})
e <- rnorm(n, 0, 1)
bw <- sapply(w, switch, group1 = .3, group2 = 0, group3 = 0)
dw <- sapply(w, switch, group1 = .5, group2 = .3, group3 = .1)
y <- 5 + .3 * x + bw + dw * x + .1 * c1 - .1 * c2 + 1 * e
dat <- data.frame(x, w, y, c1, c2)
head(dat)
psych::describe(dat)
dat$x <- dat$x + 2
dat$c1 <- dat$c1 + 3
psych::describe(dat)
summary(lm_y <- lm(y ~ x*w + c1 + c2, dat))
head(dat)
data_mod_cat <- dat
usethis::use_data(data_mod_cat, overwrite = TRUE)


# Generate data
library(lavaan)
set.seed(53151)
n <- 300
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
c1 <- ivs[, 4]
c2 <- ivs[, 5]
w1 <- sample(c("group1", "group2", "group3"), n, replace = TRUE)
w2 <- sample(c("team1", "team2"), n, replace = TRUE)
x <- sapply(w, function(x) {switch(x,
                            group1 = rnorm(1, 1),
                            group2 = rnorm(1, 1.2),
                            group3 = rnorm(1, 1.3))})
em1 <- rnorm(n, 0, 1)
em2 <- rnorm(n, 0, 1)
ey <- rnorm(n, 0, 1)
bw1 <- sapply(w1, switch, group1 = .3, group2 = 0, group3 = 0)
d1w1 <- sapply(w1, switch, group1 = .5, group2 = .3, group3 = .1)
bw2 <- sapply(w2, switch, team1 = .0, team2 = .2)
d2w2 <- sapply(w2, switch, team1 = .0, team2 = .0)
m1 <- .4 * x + bw1 + d1w1 * x + 1 * em1
m2 <- .4 * m1 + 1 * em2
y <- .4 * m2 + bw2 + d2w2 * m2 + 1 * ey
dat <- data.frame(x, w1, w2, m1, m2, y, c1, c2)
dat$x <- dat$x + 5
dat$m1 <- dat$m1 + 5
dat$m2 <- dat$m2 + 5
dat$y <- dat$y + 4
dat$c1 <- dat$c1 + 4
dat$c2 <- dat$c2 + 4
head(dat)
psych::describe(dat)
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ m1 + x + w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m2*w2 + m1 + x + w1 + c1 + c2, dat))
# dat$w1_group2 <- ifelse(dat$w1 == "group2", 1, 0)
# dat$w1_group3 <- ifelse(dat$w1 == "group3", 1, 0)
# dat$w2_team2 <- ifelse(dat$w2 == "team2", 1, 0)
# dat$x_w1_group2 <- dat$x * dat$w1_group2
# dat$x_w1_group3 <- dat$x * dat$w1_group3
# dat$m2_w2_team2 <- dat$m2 * dat$w2_team2
data_med_mod_serial_cat <- dat
usethis::use_data(data_med_mod_serial_cat, overwrite = TRUE)


# Generate data
library(lavaan)
set.seed(53253)
n <- 300
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
c1 <- ivs[, 4]
c2 <- ivs[, 5]
w1 <- sample(c("group1", "group2", "group3"), n, replace = TRUE)
w2 <- sample(c("team1", "team2"), n, replace = TRUE)
x <- sapply(w, function(x) {switch(x,
                            group1 = rnorm(1, 1),
                            group2 = rnorm(1, 1.2),
                            group3 = rnorm(1, 2.5))})
em1 <- rnorm(n, 0, 1)
em2 <- rnorm(n, 0, 1)
ey <- rnorm(n, 0, 1)
bw1 <- sapply(w1, switch, group1 = .3, group2 = 0, group3 = 0)
d1w1 <- sapply(w1, switch, group1 = .5, group2 = .3, group3 = .1)
bw2 <- sapply(w2, switch, team1 = .0, team2 = .2)
d2w2 <- sapply(w2, switch, team1 = .0, team2 = .0)
m1 <- .4 * x + bw1 + d1w1 * x + 1 * em1
m2 <- .4 * x + 1 * em2
y <- .2 + m1 + .4 * m2 + bw2 + d2w2 * m2 + 1.25 * ey
dat <- data.frame(x, w1, w2, m1, m2, y, c1, c2)
dat$x <- dat$x + 5
dat$m1 <- dat$m1 + 5
dat$m2 <- dat$m2 + 5
dat$y <- dat$y + 6
dat$c1 <- dat$c1 + 4
dat$c2 <- dat$c2 + 4
head(dat)
psych::describe(dat)
summary(lm_m1 <- lm(m1 ~ x*w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x*w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m1*w2 + m2*w2 + m1 + x + w1 + c1 + c2, dat))
# dat$w1_group2 <- ifelse(dat$w1 == "group2", 1, 0)
# dat$w1_group3 <- ifelse(dat$w1 == "group3", 1, 0)
# dat$w2_team2 <- ifelse(dat$w2 == "team2", 1, 0)
# dat$x_w1_group2 <- dat$x * dat$w1_group2
# dat$x_w1_group3 <- dat$x * dat$w1_group3
# dat$m2_w2_team2 <- dat$m2 * dat$w2_team2
data_med_mod_parallel_cat <- dat
usethis::use_data(data_med_mod_parallel_cat, overwrite = TRUE)





# Generate data
library(lavaan)
set.seed(5341)
n <- 300
ivs <- MASS::mvrnorm(n, c(0, 0, 0, 0, 0), diag(5))
c1 <- ivs[, 4]
c2 <- ivs[, 5]
w1 <- sample(c("group1", "group2", "group3"), n, replace = TRUE)
w2 <- sample(c("team1", "team2"), n, replace = TRUE)
x <- sapply(w1, function(x) {switch(x,
                            group1 = rnorm(1, 1),
                            group2 = rnorm(1, 1.2),
                            group3 = rnorm(1, 2.5))})
em11 <- rnorm(n, 0, 1)
em12 <- rnorm(n, 0, 1)
em2 <- rnorm(n, 0, 1)
ey <- rnorm(n, 0, 1)
bw1 <- sapply(w1, switch, group1 = .3, group2 = 0, group3 = 0)
d1w1 <- sapply(w1, switch, group1 = .5, group2 = .3, group3 = .1)
bw2 <- sapply(w2, switch, team1 = .0, team2 = .2)
d2w2 <- sapply(w2, switch, team1 = .0, team2 = .0)
m11 <- .4 * x + bw1 + d1w1 * x + 1 * em11
m12 <- .4 * x + 1 *em12
m2 <- .4 * x + 1 * em2
y <- .4 + m12 + .4 * m2 + bw2 + d2w2 * m2 + 1.25 * ey
dat <- data.frame(x, w1, w2, m11, m12, m2, y, c1, c2)
dat$x <- dat$x + 5
dat$m11 <- dat$m11 + 5
dat$m12 <- dat$m12 + 5
dat$m2 <- dat$m2 + 5
dat$y <- dat$y + 6
dat$c1 <- dat$c1 + 4
dat$c2 <- dat$c2 + 4
head(dat)
psych::describe(dat)
summary(lm_m11 <- lm(m11 ~ x*w1 + c1 + c2, dat))
summary(lm_m12 <- lm(m12 ~ m11 + x + w1 + c1 + c2, dat))
summary(lm_m2 <- lm(m2 ~ x + w1 + c1 + c2, dat))
summary(lm_y <- lm(y ~ m12 + m2*w2 + m12 + x + c1 + c2, dat))
data_med_mod_serial_parallel_cat <- dat
usethis::use_data(data_med_mod_serial_parallel_cat, overwrite = TRUE)
