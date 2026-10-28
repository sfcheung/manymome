q_aux_data <- function() {
  set.seed(731)
  n <- 260
  z <- rnorm(n)
  x <- rnorm(n)
  c1 <- rnorm(n)
  m1 <- .5*x + .3*c1 + .4*z + rnorm(n)
  m2 <- .3*x + .3*m1 + .3*z + rnorm(n)
  y <- .2*x + .4*m1 + .3*m2 + .2*c1 + .3*z + rnorm(n)
  d <- data.frame(x, m1, m2, y, c1, z, z2 = z + rnorm(n))
  d$m1[runif(n) < plogis(-1 + .5*z)] <- NA
  d$c1[1:8] <- NA
  d$z[9:18] <- NA
  rownames(d) <- paste0("case", sample.int(n))
  d
}

q_aux_fit <- function(data, aux = "z", ...) {
  q_mediation(x = "x", m = "m1", y = "y", cov = "c1", model = "simple",
               data = data, aux = aux, fit_method = "sem", fixed.x = FALSE,
               parallel = FALSE, progress = FALSE, ...)
}

test_that("q_mediation auxiliaries match semTools and retain the original cases", {
  d <- q_aux_data()
  for (estimator in c("ML", "MLR")) {
    q <- q_aux_fit(d, aux = c("z", "z2"), boot_ci = FALSE,
                   sem_args = list(estimator = estimator))
    ref <- do.call(semTools::auxiliary,
               list(model = "m1 ~ x + c1\ny ~ m1 + x + c1", data = d,
                    aux = c("z", "z2"), fun = "sem", estimator = estimator))
    expect_equal(lavaan::coef(get_fit(q)), lavaan::coef(ref), tolerance = 1e-6)
    expect_equal(lavaan::lavInspect(get_fit(q), "vcov"),
                 lavaan::lavInspect(ref, "vcov"), tolerance = 1e-6)
    expect_equal(q$model_matrices$model_matrix$z, d$z)
    expect_equal(lavaan::lavInspect(get_fit(q), "nobs"), nrow(d))
    pt <- lavaan::parameterTable(ref)
    expect_false(any(pt$rhs[pt$op == "~"] %in% q$aux))
    nulls <- fit_null(q$model_matrices, get_fit(q))
    dvs <- names(q$model_matrices$model_matrices)
    expect_length(nulls, length(dvs))
    null_models <- c("m1 ~ 0*x + 0*c1\ny ~ m1 + x + c1",
                     "m1 ~ x + c1\ny ~ 0*m1 + 0*x + 0*c1")
    for (i in seq_along(dvs)) {
      ref0 <- do.call(semTools::auxiliary,
                  list(model = null_models[i], data = d, aux = c("z", "z2"),
                       fun = "sem", estimator = estimator))
      expect_equal(lavaan::fitMeasures(nulls[[i]], "logl"),
                   lavaan::fitMeasures(ref0, "logl"), tolerance = 1e-6)
      nt <- lavaan::parameterEstimates(nulls[[i]])
      expect_true(all(nt$est[nt$lhs == dvs[i] & nt$op == "~"] == 0))
    }
    expect_output(print(q), "Auxiliary variable")
  }
})

test_that("auxiliary bootstrap and MC intervals agree with the separate workflow", {
  d <- q_aux_data()
  ref <- do.call(semTools::auxiliary,
                 list(model = "m1 ~ x + c1\ny ~ m1 + x + c1", data = d,
                      aux = "z", fun = "sem"))
  for (type in c("mc", "boot")) {
    R <- if (type == "mc") 1000 else 60
    q <- q_aux_fit(d, ci_type = type, R = R, seed = 23)
    for (std in c(FALSE, TRUE)) {
      ind <- indirect_effect(x = "x", m = "m1", y = "y", fit = ref,
               standardized_x = std, standardized_y = std, ci_type = type,
               R = R, seed = 23, parallel = FALSE, progress = FALSE)
      actual <- if (std) q$ind_out$stdxy else q$ind_out$ustd
      expect_equal(as.numeric(coef(actual)), as.numeric(coef(ind)), tolerance = 1e-6)
      expect_equal(as.numeric(confint(actual)), as.numeric(confint(ind)), tolerance = 1e-6)
    }
  }
})

test_that("q_mediation supports auxiliaries for each existing model form", {
  d <- q_aux_data()
  models <- list(
    parallel = "m1 ~ x + c1\nm2 ~ x + c1\ny ~ m1 + m2 + x + c1",
    serial = "m1 ~ x + c1\nm2 ~ m1 + x + c1\ny ~ m1 + m2 + x + c1")
  for (model in names(models)) {
    q <- q_mediation(x = "x", m = c("m1", "m2"), y = "y", cov = "c1",
             model = model, aux = "z", data = d, fit_method = "sem",
             boot_ci = FALSE, progress = FALSE)
    ref <- do.call(semTools::auxiliary,
                   list(model = models[[model]], data = d, aux = "z", fun = "sem"))
    actual <- lavaan::coef(get_fit(q)); expected <- lavaan::coef(ref)
    expect_equal(actual[order(names(actual))], expected[order(names(expected))], tolerance = 1e-6)
  }
  q <- q_mediation(x = "x", y = "y", model = "x -> m1 -> y", aux = "z",
            data = d, fit_method = "sem", boot_ci = FALSE, progress = FALSE)
  expect_length(q$ind_out$ustd, 1L)
  expect_true("z" %in% lavaan::lavNames(get_fit(q)))
})

test_that("auxiliary NULL preserves the output and progress controls the message", {
  d <- q_aux_data()
  args <- list(x = "x", m = "m1", y = "y", model = "simple", data = d,
               fit_method = "sem", boot_ci = FALSE, progress = FALSE)
  q0 <- do.call(q_mediation, args)
  q1 <- do.call(q_mediation, c(args, list(aux = NULL)))
  expect_identical(names(q0), names(q1))
  expect_equal(lavaan::coef(get_fit(q0)), lavaan::coef(get_fit(q1)))
  expect_equal(lavaan::lavInspect(get_fit(q0), "vcov"),
               lavaan::lavInspect(get_fit(q1), "vcov"))
  expect_equal(coef(q0$ind_out$ustd), coef(q1$ind_out$ustd))
  args$aux <- "z"
  expect_message(quiet <- do.call(q_mediation, args), NA)
  args$progress <- TRUE
  expect_message(verbose <- do.call(q_mediation, args), "fixed.x = FALSE", fixed = TRUE)
  expect_false(lavaan::lavInspect(get_fit(quiet), "options")$fixed.x)
  expect_equal(lavaan::coef(get_fit(quiet)), lavaan::coef(get_fit(verbose)))
})

test_that("invalid or unsupported auxiliary inputs are rejected", {
  d <- q_aux_data()
  expect_error(q_aux_fit(d, aux = "absent"), "all variables")
  expect_error(q_aux_fit(d, aux = "m1"), "must not also be model")
  expect_error(q_aux_fit(d, aux = "c1"), "must not also be model")
  expect_error(q_aux_fit(d, aux = c("z", "z")), "unique")
  expect_error(q_aux_fit(d, aux = character()), "nonempty")
  d$bad <- 1
  expect_error(q_aux_fit(d, aux = "bad"), "nonconstant")
  d$bad <- factor(d$z > 0)
  expect_error(q_aux_fit(d, aux = "bad"), "numeric")
  expect_error(q_aux_fit(d, missing = "listwise"), "only for SEM")
  expect_error(q_aux_fit(d, moderators = list("x -> y" = "z2")), "without indicators or moderators")
  expect_error(q_aux_fit(d, indicators = list(x = "z2")), "without indicators or moderators")
  expect_error(q_aux_fit(d, sem_args = list(estimator = "WLSMV")), "only ML and MLR")
  expect_error(q_aux_fit(d, sem_args = list(group = "z2")), "does not currently support")
})

test_that("auxiliary fits retain the existing substantive path plot", {
  skip_if_not_installed("semPlot")
  skip_if_not_installed("semptools", minimum_version = "0.3.2")
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off())
  q <- q_aux_fit(q_aux_data(), boot_ci = FALSE)
  for (standardized in c(FALSE, TRUE)) {
    p <- plot(q, plot_now = FALSE, standardized = standardized)
    expect_setequal(p$graphAttributes$Nodes$names, c("x", "m1", "y"))
  }
})
