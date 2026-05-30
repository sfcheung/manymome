#' #Create several datasets for testing purpose.
#'
#' A latent variable model
# generate data
library(lavaan)
set.seed(246)
n <- 500
fx <- rnorm(n)
fw <- rnorm(n)
fm <- (.4 + .4 * fw) * fx + .15 * rnorm(n)
fy <- .6 * fm + sqrt(1 - .6^2) * rnorm(n)
p <- 4
x <- cbind(fx) %*% rbind(rep(.8, p)) + matrix(sqrt(1 - .8^2) * rnorm(n * p), nrow = n)
w <- cbind(fw) %*% rbind(rep(.8, p)) + matrix(sqrt(1 - .8^2) * rnorm(n * p), nrow = n)
m <- cbind(fm) %*% rbind(rep(.8, p)) + matrix(sqrt(1 - .8^2) * rnorm(n * p), nrow = n)
y <- cbind(fy) %*% rbind(rep(.8, p)) + matrix(sqrt(1 - .8^2) * rnorm(n * p), nrow = n)
colnames(x) <- paste0("x", seq_len(ncol(x)))
colnames(w) <- paste0("w", seq_len(ncol(w)))
colnames(m) <- paste0("m", seq_len(ncol(m)))
colnames(y) <- paste0("y", seq_len(ncol(y)))
dat <- data.frame(
  x,
  w,
  m,
  y
)
head(dat)
mod <-
"
fx =~ x1 + x2 + x3 + x4
fw =~ w1 + w2 + w3 + w4
fm =~ m1 + m2 + m3 + m4
fy =~ y1 + y2 + y3 + y4
fm ~ fx + fw + fx:fw
fy ~ fm + fx
"
fit <- sam(
  model = mod,
  data = dat
)
summary(fit)
lavInspect(fit, "cov.lv")
lavInspect(fit, "mean.lv")

boot_out <- do_boot(
  fit,
  R = 2000,
  seed = 54321,
  parallel = TRUE,
  ncores = 20
)
mc_out <-  do_mc(
  fit,
  R = 1000,
  seed = 54321,
  parallel = TRUE,
  ncores = 20
)
out_xmy_on_w_boot <- cond_indirect_effects(
  wlevels = "fw",
  x = "fx",
  y = "fy",
  m = "fm",
  fit = fit,
  boot_ci = TRUE,
  boot_out = boot_out
)
out_xmy_on_w_boot
out_xmy_on_w_mc <- cond_indirect_effects(
  wlevels = "fw",
  x = "fx",
  y = "fy",
  m = "fm",
  fit = fit,
  mc_ci = TRUE,
  mc_out = mc_out
)
out_xmy_on_w_mc

data_sem_mome <- dat
usethis::use_data(data_sem_mome, overwrite=TRUE)
