library(manymome)
library(testthat)
suppressMessages(library(lavaan))

test_that("SAM: lavaan::lav_model_implied", {

data_sem_miss <- data_sem
# data_sem_miss[1:10, 2:14] <- NA
# data_sem_miss[11:20, c(1:3, 5:14)] <- NA
# data_sem_miss[21:30, c(1:7, 9:14)] <- NA
# data_sem_miss[31:40, c(1:10, 12:14)] <- NA
# data_sem_miss[41:50, ] <- NA

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

fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml"
)

fit2 <- sam(
  model = mod,
  data = data_sem_miss[-c(1:20), ],
  missing = "fiml"
)

# Change fit2 est to fit1 est
fit3 <- fit2
est0 <- coef(fit1)
p_free <- fit3@ParTable$free > 0
fit3@ParTable$est[p_free] <- unname(est0)
fit3@Model@GLIST <- lavaan::lav_model_set_parameters(
                      fit1@Model,
                      est0)@GLIST
implied_cov_all <- lavaan::lavInspect(fit3, "cov.all")
implied_mean_ov <- lavaan::lavInspect(fit3, "mean.ov")
implied_mean_lv <- lavaan::lavInspect(fit3, "mean.lv")
mod0 <- lavaan::lav_model_set_parameters(fit3@Model, est0)
implied_mean_ov[] <- lavaan::lav_model_implied(mod0,
                        GLIST = NULL,
                        delta = TRUE)$mean[[1]][, 1]
fit1_cov_all <- lavInspect(fit1, "cov.all")
fit1_mean_ov <- lavInspect(fit1, "mean.ov")

expect_equal(implied_cov_all,
             fit1_cov_all,
             tolerance = 1e-5)
expect_equal(implied_mean_ov,
             fit1_mean_ov,
             tolerance = 1e-5)

})

test_that("SAM: lavaan::lav_model_implied: Multigroup", {

data_sem_miss <- data_sem
# data_sem_miss[1:10, 2:14] <- NA
# data_sem_miss[11:20, c(1:3, 5:14)] <- NA
# data_sem_miss[21:30, c(1:7, 9:14)] <- NA
# data_sem_miss[31:40, c(1:10, 12:14)] <- NA
# data_sem_miss[41:50, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~  f2 + f3 + f1
"

set.seed(3456)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                           size = nrow(data_sem_miss),
                           replace = TRUE)

fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  group.label = c("gp1", "gp2"),
  missing = "fiml",
  warn = FALSE
)

fit2 <- sam(
  model = mod,
  data = data_sem_miss[-c(1:20), ],
  missing = "fiml",
  group = "gp",
  group.label = c("gp1", "gp2"),
)

# Change fit2 est to fit1 est
fit3 <- fit2
est0 <- coef(fit1)
p_free <- fit3@ParTable$free > 0
fit3@ParTable$est[p_free] <- unname(est0)
fit3@Model@GLIST <- lavaan::lav_model_set_parameters(
                      fit1@Model,
                      est0)@GLIST
implied_cov_all <- lavaan::lavInspect(fit3, "cov.all")
implied_mean_ov <- lavaan::lavInspect(fit3, "mean.ov")
implied_mean_lv <- lavaan::lavInspect(fit3, "mean.lv")
mod0 <- lavaan::lav_model_set_parameters(fit3@Model, est0)
implied_mean_ov[[1]] <- lavaan::lav_model_implied(mod0,
                          GLIST = NULL,
                          delta = TRUE)$mean[[1]][, 1]
implied_mean_ov[[2]] <- lavaan::lav_model_implied(mod0,
                          GLIST = NULL,
                          delta = TRUE)$mean[[2]][, 1]
fit1_cov_all <- lavInspect(fit1, "cov.all")
fit1_mean_ov <- lavInspect(fit1, "mean.ov")

expect_equal(implied_cov_all,
             fit1_cov_all,
             tolerance = 1e-5)
expect_equal(implied_mean_ov,
             fit1_mean_ov,
             tolerance = 1e-5,
             ignore_attr = TRUE)

})
