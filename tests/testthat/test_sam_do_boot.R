skip_on_cran()

# The functions below are only used for
# testing and updating gen_boot_i_lavaan()

library(manymome)
library(testthat)
suppressMessages(library(lavaan))

get_boot_est <- function(
  boot_out,
  boot_chk
) {
  boot_est <- lapply(
                boot_out,
                function(x) {
                  est <- x$est
                  est$label <- lav_partable_labels(est)
                  boot_est <- stats::setNames(est$est, est$label)
                  boot_est <- boot_est[colnames(boot_chk)]
                })
  boot_est <- do.call(rbind, boot_est)
  boot_est
}


# ==== Multigroup, FIML, some cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  missing = "fiml",
  warn = FALSE
)

bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 12345
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Multigroup, FIML, no cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Multigroup, listwise, some cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Multigroup, listwise, no cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
# data_sem_miss[41:50, ] <- NA
# data_sem_miss[200, ] <- NA


mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Single-group, FIML, some cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Single-group, FIML, no cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})

# ==== Single-group, listwise, some cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Single-group, listwise, no cases empty, fixed.x ====

test_that("SAM: Internal update", {

# Check only coefficient.

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
# data_sem_miss[41:50, ] <- NA
# data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13
f2 ~ f1 + x07
f3 ~ f1
f4 ~ f2 + f3 + x14
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})



# ==== Multigroup, FIML, some cases empty ====

test_that("SAM: do_boot", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~ f2 + f3
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})

# ==== Multigroup, FIML, no cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~ f2 + f3
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Multigroup, listwise, some cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~  f2 + f3
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Multigroup, listwise, no cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
# data_sem_miss[41:50, ] <- NA
# data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~  f2 + f3
"

set.seed(5981470)
data_sem_miss$gp <- sample(c("gp1", "gp2"),
                      size = nrow(data_sem_miss),
                      replace = TRUE)

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  group = "gp",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Single-group, FIML, some cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~ f2 + f3
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})

# ==== Single-group, FIML, no cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~ f2 + f3
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  missing = "fiml",
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Single-group, listwise, some cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
data_sem_miss[41:50, ] <- NA
data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~  f2 + f3
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})


# ==== Single-group, listwise, no cases empty ====

test_that("SAM: Internal update", {

skip("Not used for now")
# Expected to be different from bootstrapLavaan

data_sem_miss <- data_sem
data_sem_miss[1:10, 2:14] <- NA
data_sem_miss[11:20, c(1:3, 5:14)] <- NA
data_sem_miss[21:30, c(1:7, 9:14)] <- NA
data_sem_miss[31:40, c(1:10, 12:14)] <- NA
# data_sem_miss[41:50, ] <- NA
# data_sem_miss[200, ] <- NA

mod <-
"
f1 =~ x01 + x02 + x03
f2 =~ x04 + x05 + x06 + x07
f3 =~ x08 + x09 + x10
f4 =~ x11 + x12 + x13 + x14
f2 ~ f1
f3 ~ f1
f4 ~  f2 + f3
"

# The warning is expected
fit1 <- sam(
  model = mod,
  data = data_sem_miss,
  warn = FALSE
)

boot_chk1 <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345
)

boot_out <- do_boot(
  fit = fit1,
  R = 2,
  seed = 2345,
  parallel = FALSE,
  progress = !is_testing()
)

boot_est <- get_boot_est(boot_out, boot_chk1)

expect_equal(boot_est,
             boot_chk1,
             ignore_attr = TRUE,
             tolerance = 1e-5)

})
