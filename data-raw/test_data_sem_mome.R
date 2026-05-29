#' #Create several datasets for testing purpose.
#'
#' A latent variable model
# generate data
library(lavaan)
set.seed(246426)
n <- 500
fx <- rnorm(n)
fw <- rnorm(n)
fm <- (.4 + .2 * fw) * fx + .25 * rnorm(n)
fy <- .4 * fm + sqrt(1 - .4^2) * rnorm(n)
p <- 4
x <- cbind(fx) %*% rbind(rep(.7, p)) + matrix(sqrt(1 - .7^2) * rnorm(n * p), nrow = n)
w <- cbind(fw) %*% rbind(rep(.7, p)) + matrix(sqrt(1 - .7^2) * rnorm(n * p), nrow = n)
m <- cbind(fm) %*% rbind(rep(.7, p)) + matrix(sqrt(1 - .7^2) * rnorm(n * p), nrow = n)
y <- cbind(fy) %*% rbind(rep(.7, p)) + matrix(sqrt(1 - .7^2) * rnorm(n * p), nrow = n)
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

data_sem_mome <- dat
usethis::use_data(data_sem_mome, overwrite=TRUE)
