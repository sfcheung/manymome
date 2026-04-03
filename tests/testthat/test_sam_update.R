skip("WIP")

library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("SAM: Internal update", {

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ a2*f1
f3 ~ a3*f1
f4 ~  b2*f2 + b3*f3 + cp*f1
a2b2 := a2 * b2
a3b3 := a3 * b3
"

# The warning is expected
fit <- sam(
  model = mod,
  data = data_sem,
  warn = FALSE
)

fit.sem <- sem(
  model = mod,
  data = data_sem
)

fit@internal$sam.cmd
fit@internal$sam.mm.list
fit@internal$sam.mm.args
fit@internal$sam.struc.args
fit@internal$sam.method
fit@internal$sam.lavoptions
fit@internal$sam.lavoptions$se
fit@internal$sam.local.options
fit@internal$sam.global.options

dat_used <- lavInspect(fit, "data")
n <- nrow(dat_used)
set.seed(1234)
dat2 <- dat_used[sample.int(n, replace = FALSE), ]
expect_equal(colMeans(dat_used),
             colMeans(dat2))
expect_equal(colMeans(data_sem),
             colMeans(dat2))

# fit2 <- sam(
#   model = fit,
#   data = dat2,
#   warn = FALSE
# )

fit_edited <- fit

# From the help page of lav_data_update
lavdata <- fit@Data
lavoptions <- lavInspect(fit, "options")

# create bootstrap sample
set.seed(1234)
boot.idx <- sample(x = nobs(fit), size = nobs(fit), replace = TRUE)
newX <- list(lavdata@X[[1]][boot.idx,])

# generate update lavdata object
newdata <- lav_data_update(lavdata = lavdata, newX = newX,
                           lavoptions = lavoptions)
str(newdata)

fit_edited@Data <- newdata

# From the help page of lav_samplestats_from_data
newsampleStats <- lav_samplestats_from_data(lavdata = newdata,
                                         lavoptions = lavoptions)
fit_edited@SampleStats <- newsampleStats

fit2 <- sam(
  model = fit_edited,
  warn = FALSE
)

dat2 <- data_sem[sample.int(n), ]
colnames(dat2) <- colnames(data_sem)
head(dat2)
head(lavInspect(fit2, "data"))
tail(dat2)
tail(lavInspect(fit2, "data"))
colMeans(dat2)
colMeans(newX[[1]])

dat2 <- lavInspect(fit2, "data")
fit3 <- sam(
  model = mod,
  data = dat2,
  warn = FALSE
)

head(lavInspect(fit2, "data"))
head(lavInspect(fit3, "data"))
head(lavInspect(fit, "data"))
colMeans(data_sem)
colMeans(dat2)
colMeans(lavInspect(fit, "data"))
colMeans(lavInspect(fit2, "data"))
colMeans(lavInspect(fit3, "data"))

fit
fit3
fit2

expect_equal(coef(fit2),
             coef(fit3),
             tolerance = 1e-5)
expect_equal(coef(fit),
             coef(fit3),
             tolerance = 1e-5)

# The warning is expected
fitb <- sam(
  model = mod,
  data = data_sem_miss,
  se = "bootstrap",
  bootstrap.args = list(R = 2),
  missing = "fiml",
  warn = FALSE
)

fitb@internal$sam.lavoptions$se
fitb@Options$bootstrap

})
