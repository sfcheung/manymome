library(testthat)
library(manymome)

test_that("indicators", {

ind <- list(
  x = c("x1", "x2", "x3"),
  y = c("y1", "y2", "y3"),
  m1 = c("m11", "m12", "m13", "m14"),
  m2 = c("m21", "m22", "m23")
)

out <- measurement_syntax(ind)

chk <- "x =~ x1 + x2 + x3\ny =~ y1 + y2 + y3\nm1 =~ m11 + m12 + m13 + m14\nm2 =~ m21 + m22 + m23"

expect_equal(out,
             chk)

dat <- data.frame(x1 = 1:10,
                  x2 = 1:10 + 3,
                  x3 = 1:10 + 5,
                  y1 = 1:10 + 10,
                  y2 = 20 - 1:10,
                  y3 = 1:10 + 50,
                  m = 1:10 + 1)

expect_no_error(check_indicators(
                  list(x = c("x1", "x2", "x3")),
                  dat))
expect_error(check_indicators(
                  list(x = c("x1", "x4", "x3")),
                  dat),
             "x4")
expect_error(check_indicators(
                  list(m = c("x1", "x2", "x3")),
                  dat),
             "m")

inds <- list(
  x = c("x1", "x3", "x2"),
  y = c("y1", "y2", "y3")
)

inds_rev <- list(
  x = c("x1", "-x3", "x2"),
  y = c("y1", "y2", "y3")
)

dat_rev <- dat
dat_rev$x3 <- -1 * dat_rev$x3

expect_no_error(check_indicators(inds_rev, dat))

chk0 <- reverse_indicators(inds_rev)
expect_identical(chk0$x, "x3")
expect_identical(chk0$y, character(0))

dat_scores_rev <- scale_scores(
  indicators = inds_rev,
  data = dat
)

expect_equal(dat_scores_rev$x,
             rowMeans(dat_rev[, inds$x]))
expect_equal(dat_scores_rev$y,
             rowMeans(dat_rev[, inds$y]))

dat_scores <- scale_scores(
  indicators = inds,
  data = dat,
  score_fun = sum
)

expect_equal(dat_scores$x,
             rowSums(dat[, inds$x]))
expect_equal(dat_scores$y,
             rowSums(dat[, inds$y]))

dat_scores <- scale_scores(
  indicators = inds,
  data = dat,
  score_fun = list(x = sum, y = mean)
)

expect_equal(dat_scores$x,
             rowSums(dat[, inds$x]))
expect_equal(dat_scores$y,
             rowMeans(dat[, inds$y]))

dat_scores <- scale_scores(
  indicators = inds,
  data = dat,
  score_fun = list(y = sum, x = mean)
)

expect_equal(dat_scores$y,
             rowSums(dat[, inds$y]))
expect_equal(dat_scores$x,
             rowMeans(dat[, inds$x]))

dat2 <- dat
dat2[1, 1:3] <- NA
dat2[2, 1:3 + 3] <- NA
dat2[3, 1] <- NA
dat2[4, 4] <- NA

dat_scores <- scale_scores(
  indicators = inds,
  data = dat2
)

expect_equal(dat_scores$x,
             rowMeans(dat2[, inds$x]))
expect_equal(dat_scores$y,
             rowMeans(dat2[, inds$y]))

dat_scores <- scale_scores(
  indicators = inds,
  data = dat2,
  score_args = list(na.rm = TRUE)
)

expect_equal(dat_scores$x,
             rowMeans(dat2[, inds$x], na.rm = TRUE))
expect_equal(dat_scores$y,
             rowMeans(dat2[, inds$y], na.rm = TRUE))

dat_scores <- scale_scores(
  indicators = inds,
  data = dat2,
  score_args = list(y = list(na.rm = TRUE),
                    x = list())
)

expect_equal(dat_scores$x,
             rowMeans(dat2[, inds$x], na.rm = FALSE))
expect_equal(dat_scores$y,
             rowMeans(dat2[, inds$y], na.rm = TRUE))


})
