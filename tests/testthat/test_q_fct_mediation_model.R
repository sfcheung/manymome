skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

test_that("q function: model", {

tmp1 <- paths_to_models(c("x1 -> m11 -> m12 -> y1",
                          "x1 -> m2 -> y1"))
tmp2 <- paths_to_models(list(
                            c("x1", "m11"),
                            c("m11", "m12", "y1"),
                            c("m11", "m2", "y1")
                          ))

out1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> m2 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            ci_type = "mc",
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
out1

chk1 <- c(m11 = "m11 ~ x1 + c1 + c2", m12 = "m12 ~ m11 + c1 + c2", y1 = "y1 ~ m12 + m2 + c1 + c2",
m2 = "m2 ~ x1 + c1 + c2")

expect_equal(sort(out1$lm_form),
             sort(chk1))

out2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = list(c("x1", "m11"),
                         c("m11", "m12", "y1"),
                         c("m11", "m2", "y1"),
                         c("x1", "y1")),
            cov = list(m11 = c("c1", "c2"),
                       m12 = c("c1"),
                       m2 = c("c2")),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            ci_typ = "mc",
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
out2

chk2 <- c(m11 = "m11 ~ x1 + c1 + c2", m12 = "m12 ~ m11 + c1", y1 = "y1 ~ m12 + m2 + x1",
m2 = "m2 ~ m11 + c2")

expect_equal(sort(out2$lm_form),
             sort(chk2))


out1lm <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12 -> y1",
                      "x1 -> m2 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
out1lm

chk1 <- c(m11 = "m11 ~ x1 + c1 + c2", m12 = "m12 ~ m11 + c1 + c2", y1 = "y1 ~ m12 + m2 + c1 + c2",
m2 = "m2 ~ x1 + c1 + c2")

expect_equal(sort(out1lm$lm_form),
             sort(chk1))


out2lm <- q_mediation(
            x = "x1",
            y = "y1",
            model = list(c("x1", "m11"),
                         c("m11", "m12", "y1"),
                         c("m11", "m2", "y1"),
                         c("x1", "y1")),
            cov = list(m11 = c("c1", "c2"),
                       m12 = c("c1"),
                       m2 = c("c2")),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
out2lm

chk2 <- c(m11 = "m11 ~ x1 + c1 + c2", m12 = "m12 ~ m11 + c1", y1 = "y1 ~ m12 + m2 + x1",
m2 = "m2 ~ m11 + c2")

expect_equal(sort(out2lm$lm_form),
             sort(chk2))

})

test_that("q function: model: special cases", {

out1 <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            ci_type = "mc",
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
out1

chk1 <- c(y1 = "y1 ~ x1 + c1 + c2")

expect_equal(sort(out1$lm_form),
             sort(chk1))

out2 <- q_mediation(
            x = "x1",
            y = "y1",
            model = list(c("x1", "m11"),
                         c("x1", "y1")),
            cov = list(m11 = c("c1", "c2"),
                       m12 = c("c1"),
                       m2 = c("c2")),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            ci_typ = "mc",
            fit_method = "sem",
            parallel = FALSE,
            progress = FALSE
          )
out2

chk2 <-c(m11 = "m11 ~ x1 + c1 + c2", y1 = "y1 ~ x1")

expect_equal(sort(out2$lm_form),
             sort(chk2))


out1lm <- q_mediation(
            x = "x1",
            y = "y1",
            model = c("x1 -> m11 -> m12",
                      "x1 -> y1"),
            cov = c("c1", "c2"),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
out1lm

chk1 <- c(m11 = "m11 ~ x1 + c1 + c2", m12 = "m12 ~ m11 + c1 + c2", y1 = "y1 ~ x1 + c1 + c2")

expect_equal(sort(out1lm$lm_form),
             sort(chk1))


out2lm <- q_mediation(
            x = "x1",
            y = "y1",
            model = list(c("x1", "m11"),
                         c("x1", "y1")),
            cov = list(m11 = c("c1", "c2"),
                       m12 = c("c1"),
                       m2 = c("c2")),
            data = data_med_complicated,
            R = 200,
            seed = 1234,
            parallel = FALSE,
            progress = FALSE
          )
out2lm

chk2 <- c(m11 = "m11 ~ x1 + c1 + c2", y1 = "y1 ~ x1")

expect_equal(sort(out2lm$lm_form),
             sort(chk2))

})
