library(testthat)
library(manymome)

test_that("indicator: model syntax", {

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
                  x2 = 1:10,
                  x3 = 1:10,
                  y1 = 1:10,
                  y2 = 1:10,
                  y3 = 1:10,
                  m = 1:10)

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

})
