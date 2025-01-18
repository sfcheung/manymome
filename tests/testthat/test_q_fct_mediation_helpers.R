library(testthat)
library(manymome)

test_that("lm_listwise", {
  dat <- data.frame(x = c(1:5, 1:4, NA),
                    m = c(NA, 1:4, 1:5),
                    y = c(1, NA, 3:5, 1:5),
                    c1 = c(1:4, NA, 1:5))
  formulas0 <- c("m ~ x + y",
                 "y ~ m + x")
  to_drop <- lm_listwise(formulas = formulas0,
                         data = dat)
  chk0 <- stats::na.omit(dat[, c("x", "m", "y")])
  chk1 <- attr(chk0,
               "na.action")
  chk1 <- as.integer(chk1)
  expect_setequal(to_drop,
                  chk1)
  # No case to delete
  expect_equal(lm_listwise(formulas = formulas0,
                           chk0),
               integer(0))
  # Some variables not in the data frame
  formulas1 <- c("m ~ x + y",
                 "y ~ m + w")
  out <- check_vars_lm(formulas = formulas1,
                       data = dat)
  expect_false(out)
  expect_equal(attr(out, "not_in_data"),
               "w")
  expect_error(lm_listwise(formulas = formulas1,
                           data = dat))
})
