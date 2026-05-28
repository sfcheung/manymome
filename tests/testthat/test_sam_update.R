skip_on_cran()

# The functions below are only used for
# testing and updating gen_boot_i_lavaan()

library(manymome)
library(testthat)
suppressMessages(library(lavaan))

# ==== sam_update_internal_i ====

# Input:
# - An output of sam()
# - boot_idx (row numbers in each group)
# Output:
# - An updated sam output
sam_update_internal_i <- function(
  object,
  boot_idx = NULL
) {
  lavmodel <- object@Model
  lavdata <- object@Data
  lavoptions <- lavaan::lavInspect(
                  object,
                  "options"
                )
  newX <- mapply(
            function(x, y) {
              x[y, , drop = FALSE]
            },
            x = lavdata@X,
            y = boot_idx,
            SIMPLIFY = FALSE
          )
  newdata <- lavaan::lav_data_update(
              lavdata = lavdata,
              newX = newX,
              BOOT.idx = boot_idx,
              lavoptions = lavoptions
            )
  newsampleStats <- lavaan::lav_samplestats_from_data(
                        lavdata = newdata,
                        lavoptions = lavoptions
                      )

  # Based on lavaan:::lav_sam_step0()

  x <- object

  x@Options$do.fit <- FALSE
  if (x@internal$sam.method %in%
      c("local", "fsr", "cfsr")) {
    x@Options$sample.icov <- TRUE
  }
  x@Options$se <- "none"
  x@Options$test <- "none"
  x@Options$ceq.simple <- TRUE
  x@Options$check.lv.interaction <- FALSE
  if (x@Options$se %in%
      c("local", "ij", "twostep.robust")) {
    x@Options$sample.icov <- TRUE
    x@Options$NACOV <- TRUE
    x@Options$fixed.x <- FALSE
    x@Options$ov.order <- "force.model"
  }
  # Any lv interaction terms?
  if (length(lavaan::lavNames(x, "lv.interaction")) > 0L) {
    x@Options$meanstructure  <- TRUE
  }

  x@internal <- list()

  # Adapted from bootstrapLavaan()

  if (lavmodel@fixed.x
      &&
      (length(lavaan::lavNames(object, "ov.x")) > 0L)) {
    model_boot <- NULL
  } else {
    model_boot <- lavmodel
  }

  x <- lavaan::lavaan(
    slotData = newdata,
    slotSampleStats = newsampleStats,
    slotModel = model_boot,
    slotOptions = x@Options,
    slotParTable = x@ParTable
  )

  x@internal <- object@internal

  if ((x@Model@categorical) &&
      (x@Options$se == "twostep")) {
    if (x@internal$sam.method == "local") {
      x@Options$se <- "twostep.robust"
    }
  }
  PT <- x@ParTable
  PT$est <- PT$ustart
  if (any(PT$exo > 0L)) {
    PT$est[PT$exo > 0L] <- PT$start[PT$exo > 0L]
  }
  PT$se <- rep(as.numeric(NA), length(PT$lhs))
  PT$se[(PT$free == 0L) & (!is.na(PT$ustart))] <- 0.0
  x@ParTable <- PT

  out <- lavaan::sam(
    model = x
  )
  out
}

# ==== gen_boot_idx ====

# Input:
# - A lavaan object
# Output:
# - A list of boot_idx (row numbers)
gen_boot_idx <- function(
  object,
  R = 100
) {
  fit_data <- lavaan::lavInspect(
                object,
                "data",
                drop.list.single.group = FALSE
              )
  case_idx <- lavInspect(
                object,
                "case.idx",
                drop.list.single.group = FALSE
              )
  empty_idx <- lavInspect(
                object,
                "empty.idx",
                drop.list.single.group = FALSE)
  fit_data <- mapply(
    function(x, y) {
      rownames(x) <- y
      x
    },
    x = fit_data,
    y = case_idx,
    SIMPLIFY = FALSE
  )
  case_idx2 <- mapply(
    function(x, y) {
      if (length(y) > 0) {
        return(x[-y])
      }
      x
    },
    x = case_idx,
    y = empty_idx,
    SIMPLIFY = FALSE
  )
  boot_idx <- replicate(
    n = R,
    lapply(case_idx2,
           function(x) {
            sample(x,
                   size = length(x),
                   replace = TRUE)
           }),
    simplify = FALSE
  )
  fit_data_rownames <- lapply(
    fit_data,
    rownames
  )
  boot_idx2 <- mapply(
    function(x, y) {
      mapply(
        function(xx, yy) {
          match(as.character(xx), yy)
        },
        x = x,
        y = y,
        SIMPLIFY = FALSE
      )
    },
    x = boot_idx,
    MoreArgs = list(y = fit_data_rownames),
    SIMPLIFY = FALSE
  )
  boot_idx2
}

# ==== Multigroup, FIML, some cases empty ====

test_that("SAM: Internal update", {

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


# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    x0 <- x[-y]
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- mapply(
  function(x, y) {
    x <- as.data.frame(x)
    x$gp <- y
    x
  },
  x = fit1_data_boot,
  y = names(fit1_data_boot),
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)
fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  group = "gp",
  missing = "fiml",
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})

# ==== Multigroup, FIML, no cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    if (length(y) > 0) {
      x0 <- x[-y]
    } else {
      x0 <- x
    }
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- mapply(
  function(x, y) {
    x <- as.data.frame(x)
    x$gp <- y
    x
  },
  x = fit1_data_boot,
  y = names(fit1_data_boot),
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)
fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  group = "gp",
  missing = "fiml",
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})


# ==== Multigroup, listwise, some cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    if (length(y) > 0) {
      x0 <- x[-y]
    } else {
      x0 <- x
    }
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- mapply(
  function(x, y) {
    x <- as.data.frame(x)
    x$gp <- y
    x
  },
  x = fit1_data_boot,
  y = names(fit1_data_boot),
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)
fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  group = "gp",
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})


# ==== Multigroup, listwise, no cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    if (length(y) > 0) {
      x0 <- x[-y]
    } else {
      x0 <- x
    }
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- mapply(
  function(x, y) {
    x <- as.data.frame(x)
    x$gp <- y
    x
  },
  x = fit1_data_boot,
  y = names(fit1_data_boot),
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)
fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  group = "gp",
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})


# ==== Single-group, FIML, some cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx,
  SIMPLIFY = FALSE
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    x0 <- x[-y]
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx,
  SIMPLIFY = FALSE
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data,
  SIMPLIFY = FALSE
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx,
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  missing = "fiml",
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})

# ==== Single-group, FIML, no cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx,
  SIMPLIFY = FALSE
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    if (length(y) > 0) {
      x0 <- x[-y]
    } else {
      x0 <- x
    }
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx,
  SIMPLIFY = FALSE
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data,
  SIMPLIFY = FALSE
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx,
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  missing = "fiml",
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})


# ==== Single-group, listwise, some cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx,
  SIMPLIFY = FALSE
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    if (length(y) > 0) {
      x0 <- x[-y]
    } else {
      x0 <- x
    }
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx,
  SIMPLIFY = FALSE
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data,
  SIMPLIFY = FALSE
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx,
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})


# ==== Single-group, listwise, no cases empty ====

test_that("SAM: Internal update", {

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

# Create the test dataset

fit1_data <- lavInspect(fit1,
                        "data",
                        drop.list.single.group = FALSE)
case_idx <- lavInspect(fit1,
                       "case.idx",
                       drop.list.single.group = FALSE)
lapply(case_idx, range)
sapply(case_idx, length)
empty_idx <- lavInspect(fit1,
                       "empty.idx",
                       drop.list.single.group = FALSE)
sapply(empty_idx, length)
(ns <- sapply(fit1_data, nrow))
fit1_data <- mapply(
  function(x, y) {
    rownames(x) <- y
    x
  },
  x = fit1_data,
  y = case_idx,
  SIMPLIFY = FALSE
)
ns <- sapply(fit1_data, nrow)

set.seed(234)
boot_idx <- mapply(
  function(x, y) {
    if (length(y) > 0) {
      x0 <- x[-y]
    } else {
      x0 <- x
    }
    sample(x0,
           size = length(x0),
           replace = TRUE)
  },
  x = case_idx,
  y = empty_idx,
  SIMPLIFY = FALSE
)
lapply(boot_idx, range)
sapply(boot_idx, length)

boot_idx2 <- mapply(
  function(x, y) {
    match(as.character(x), rownames(y))
  },
  x = boot_idx,
  y = fit1_data,
  SIMPLIFY = FALSE
)

# Create the dataset for the check
fit1_data_boot <- mapply(
  function(x, idx) {
    x[as.character(idx), , drop = FALSE]
  },
  x = fit1_data,
  idx = boot_idx,
  SIMPLIFY = FALSE
)
sapply(fit1_data_boot, nrow)

fit1_data_boot <- do.call(rbind, fit1_data_boot)
row.names(fit1_data_boot) <- NULL
nrow(fit1_data_boot)
nrow(na.omit(fit1_data_boot))
nrow(na.omit(do.call(rbind, fit1_data)))

fit1_chk <- sam(
  model = mod,
  data = fit1_data_boot,
  warn = FALSE
)
# Must be equal
lavInspect(fit1_chk, "nobs")
lavInspect(fit1, "nobs")

# Update sam

set.seed(234)
boot_idx_update <- gen_boot_idx(fit1, R = 1)[[1]]

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

expect_equal(coef(fit1_chk),
             coef(fit1_updated),
             tolerance = 1e-4)

expect_equal(vcov(fit1_chk),
             vcov(fit1_updated),
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.struc.fit,
             fit1_updated@internal$sam.struc.fit,
             tolerance = 1e-4)

expect_equal(fit1_chk@internal$sam.mm.rel,
             fit1_updated@internal$sam.mm.rel,
             tolerance = 1e-4)

})

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  gp1 = tmp1[[1]][1, ],
  gp2 = tmp1[[2]][1, ]
)

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  gp1 = tmp1[[1]][1, ],
  gp2 = tmp1[[2]][1, ]
)

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  gp1 = tmp1[[1]][1, ],
  gp2 = tmp1[[2]][1, ]
)

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  gp1 = tmp1[[1]][1, ],
  gp2 = tmp1[[2]][1, ]
)

fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
)

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  tmp1[[1]][1, ]
)

suppressWarnings(fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
))

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  tmp1[[1]][1, ]
)

suppressWarnings(fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
))

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  tmp1[[1]][1, ]
)

suppressWarnings(fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
))

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

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

chk <- bootstrapLavaan(
  fit1,
  R = 2,
  iseed = 2345,
  keep.idx = TRUE
)

# Update sam

tmp1 <- attr(chk, "boot.idx")
boot_idx2 <- list(
  tmp1[[1]][1, ]
)

suppressWarnings(fit1_updated <- sam_update_internal_i(
  fit1,
  boot_idx = boot_idx2
))

pnames <- colnames(chk)
expect_equal(chk[1, ],
             coef(fit1_updated)[pnames],
             tolerance = 1e-4)

})

