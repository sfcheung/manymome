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
data_sem_miss[200, ] <- NA

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

sam_update_internal_i <- function(
  object,
  boot_idx = NULL
) {
  lavdata <- object@Data
  lavoptions <- lavaan::lavInspect(
                  object,
                  "options"
                )
  newX <- mapply(
            function(x, y) {
              x[y, ]
            },
            x = lavdata@X,
            y = boot_idx
          )
  newdata <- lavaan::lav_data_update(
              lavdata = lavdata,
              newX = newX,
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

  x <- lavaan::lavaan(
    slotData = newdata,
    slotSampleStats = newsampleStats,
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
