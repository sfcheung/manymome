make_aux_data <- function() {
  set.seed(731)
  n <- 260
  z <- rnorm(n)
  c1 <- rnorm(n)
  x <- rnorm(n)
  m1 <- .5*x + .3*c1 + .4*z + rnorm(n)
  m2 <- .3*x + .3*m1 + .3*z + rnorm(n)
  y <- .2*x + .4*m1 + .3*m2 + .2*c1 + .3*z + rnorm(n)
  d <- data.frame(x, m1, m2, y, c1, z)
  d$m1[runif(n) < plogis(-1 + .5*z)] <- NA
  d$c1[1:8] <- NA
  d$z[9:18] <- NA
  rownames(d) <- paste0("case", sample(seq_len(n)))
  d
}

aux_quick <- function(data, ...) {
  q_simple_mediation(x = "x", m = "m1", y = "y", cov = "c1",
                     data = data, fit_method = "sem", fixed.x = FALSE,
                     progress = FALSE, parallel = FALSE, ...)
}

test_that("auxiliary FIML matches an independently specified model", {
  d <- make_aux_data()
  q <- aux_quick(d, aux = "z", boot_ci = FALSE)
  f <- do.call(semTools::auxiliary, list(model = "m1 ~ x + c1\ny ~ m1 + x + c1",
                           data = d, aux = "z", fun = "sem",
                           fixed.x = FALSE, missing = "fiml"))
  expect_equal(lavaan::coef(get_fit(q)), lavaan::coef(f), tolerance = 1e-7)
  expect_equal(q$model_matrices$model_matrix$z, d$z)
  expect_equal(lavaan::lavInspect(get_fit(q), "nobs"), nrow(d))
  pt <- lavaan::parameterTable(get_fit(q))
  expect_false(any(pt$rhs[pt$op == "~"] == "z"))
  expect_equal(length(q$ind_out$ustd), 1)
  # Null models used for R-square tests must retain all nuisance parameters.
  nulls <- fit_null(q$model_matrices, get_fit(q))
  for (nm in names(nulls)) {
    nt <- lavaan::parameterTable(nulls[[nm]])
    expect_true("z" %in% lavaan::lavNames(nulls[[nm]], "ov"))
    expect_true(all(nt$free[nt$lhs == nm & nt$op == "~"] == 0))
  }
  expect_output(print(q), "Auxiliary variable")
})

test_that("bootstrap and MC intervals use the augmented model", {
  d <- make_aux_data()
  f <- do.call(semTools::auxiliary, list(model = "m1 ~ x + c1\ny ~ m1 + x + c1",
                           data = d, aux = "z", fun = "sem",
                           fixed.x = FALSE, missing = "fiml"))
  for (type in c("mc", "boot")) {
    R <- if (type == "mc") 1000 else 60
    q <- aux_quick(d, aux = "z", ci_type = type, R = R, seed = 23)
    ref <- indirect_effect(x = "x", m = "m1", y = "y", fit = f,
                            ci_type = type, R = R, seed = 23,
                            parallel = FALSE, progress = FALSE)
    expect_equal(as.numeric(coef(q$ind_out$ustd)), as.numeric(coef(ref)),
                 tolerance = 1e-6)
    expect_equal(as.numeric(confint(q$ind_out$ustd)), as.numeric(confint(ref)),
                 tolerance = 1e-6)
    ref_std <- indirect_effect(x = "x", m = "m1", y = "y", fit = f,
                               standardized_x = TRUE, standardized_y = TRUE,
                               ci_type = type, R = R, seed = 23,
                               parallel = FALSE, progress = FALSE)
    expect_equal(as.numeric(coef(q$ind_out$stdxy)), as.numeric(coef(ref_std)),
                 tolerance = 1e-6)
    expect_equal(as.numeric(confint(q$ind_out$stdxy)),
                 as.numeric(confint(ref_std)), tolerance = 1e-6)
  }
})

test_that("all quick model forms forward aux", {
  d <- make_aux_data()
  forms <- list(
    parallel = "m1 ~ x + c1\nm2 ~ x + c1\ny ~ m1 + m2 + x + c1",
    serial = "m1 ~ x + c1\nm2 ~ m1 + x + c1\ny ~ m1 + m2 + x + c1"
  )
  for (form in names(forms)) {
    fun <- switch(form, parallel = q_parallel_mediation,
                   serial = q_serial_mediation)
    q <- fun(x = "x", m = c("m1", "m2"), y = "y", cov = "c1",
              aux = "z", data = d, fit_method = "sem", fixed.x = FALSE,
              boot_ci = FALSE, progress = FALSE)
    expect_equal(q$aux, "z")
    expect_true("z" %in% lavaan::lavNames(get_fit(q)))
    ref <- do.call(semTools::auxiliary,
                    list(model = forms[[form]], data = d, aux = "z",
                         fun = "sem", fixed.x = FALSE, missing = "fiml"))
    actual <- lavaan::coef(get_fit(q))
    expected <- lavaan::coef(ref)
    expect_equal(actual[order(names(actual))], expected[order(names(expected))],
                  tolerance = 1e-6)
  }
  q <- q_mediation(x = "x", y = "y", model = c("x -> m1 -> y", "x -> m2 -> y"),
                    cov = "c1", aux = "z", data = d, fit_method = "sem",
                    fixed.x = FALSE, boot_ci = FALSE, progress = FALSE)
  expect_equal(q$aux, "z")
  expect_message(q_simple_mediation(x = "x", m = "m1", y = "y", aux = "z",
                                    data = d, fit_method = "sem", boot_ci = FALSE,
                                    progress = FALSE), "fixed.x = FALSE", fixed = TRUE)
})

test_that("invalid and unsupported auxiliary specifications fail clearly", {
  d <- make_aux_data()
  expect_error(aux_quick(d, aux = "absent"), "all variables")
  expect_error(aux_quick(d, aux = "m1"), "must not also be model")
  expect_error(aux_quick(d, aux = "c1"), "must not also be model")
  expect_error(aux_quick(d, aux = c("z", "z")), "unique")
  expect_error(aux_quick(d, aux = character()), "nonempty")
  d$bad <- factor(rep(c("a", "b"), length.out = nrow(d)))
  expect_error(aux_quick(d, aux = "bad"), "numeric")
  d$bad <- NA_real_
  expect_error(aux_quick(d, aux = "bad"), "nonconstant")
  d$bad <- 1
  expect_error(aux_quick(d, aux = "bad"), "nonconstant")
  d$bad <- d$z; d$bad[1] <- Inf
  expect_error(aux_quick(d, aux = "bad"), "finite")
  expect_error(aux_quick(d, aux = "z", missing = "listwise"), "only for SEM")
  expect_error(q_simple_mediation(x = "x", m = "m1", y = "y", aux = "z",
                                  data = d, progress = FALSE), "only for SEM")
  expect_error(aux_quick(d, aux = "z", moderators = list("x -> y" = "c1")),
                "without indicators or moderators")
  expect_error(aux_quick(d, aux = "z", sem_args = list(estimator = "WLSMV")),
                "only ML and MLR")
  expect_error(aux_quick(d, aux = "z", sem_args = list(group = "c1")),
                "does not currently support")
})

test_that("aux NULL does not change the existing workflow", {
  d <- make_aux_data()
  q0 <- aux_quick(d, boot_ci = FALSE)
  q1 <- aux_quick(d, aux = NULL, boot_ci = FALSE)
  expect_equal(lavaan::coef(get_fit(q0)), lavaan::coef(get_fit(q1)))
  expect_equal(coef(q0$ind_out$ustd), coef(q1$ind_out$ustd))
  expect_null(q1$aux)
})

test_that("multiple auxiliaries and MLR retain the augmented likelihood", {
  d <- make_aux_data()
  set.seed(44)
  d$z2 <- .4*d$y + rnorm(nrow(d))
  q <- aux_quick(d, aux = c("z", "z2"), sem_args = list(estimator = "MLR"),
                  ci_type = "mc", R = 1000, seed = 34)
  expect_true(lavaan::lavInspect(get_fit(q), "converged"))
  expect_true(all(c("z", "z2") %in% lavaan::lavNames(get_fit(q))))
  expect_true(all(is.finite(confint(q$ind_out$ustd))))
  d$c1 <- factor(d$c1 > 0)
  expect_error(aux_quick(d, aux = "z"), "covariates must also be numeric")
})

test_that("path plots retain only substantive nodes", {
  skip_if_not_installed("semPlot")
  skip_if_not_installed("semptools", minimum_version = "0.3.2")
  grDevices::pdf(file = NULL)
  withr::defer(grDevices::dev.off())
  q <- aux_quick(make_aux_data(), aux = "z", boot_ci = FALSE)
  for (standardized in c(FALSE, TRUE)) {
    p <- plot(q, plot_now = FALSE, standardized = standardized)
    expect_setequal(p$graphAttributes$Nodes$names, c("x", "m1", "y"))
  }
})
