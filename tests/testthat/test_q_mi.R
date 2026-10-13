skip_if_not_installed("lavaan.mi")
skip_if_not_installed("mice")

mi_raw_data <- function() {
  set.seed(317)
  n <- 160
  x <- rnorm(n); c1 <- rnorm(n); z <- rnorm(n)
  m1 <- .5*x + .3*c1 + .4*z + rnorm(n)
  m2 <- .4*x + .3*m1 + rnorm(n)
  y <- .2*x + .4*m1 + .3*m2 + .2*c1 + rnorm(n)
  d <- data.frame(x, m1, m2, y, c1, z, id=seq_len(n))
  d$m1[runif(n) < plogis(-1+.5*z)] <- NA
  d$c1[1:5] <- NA
  d
}

mi_quick <- function(data, ...) {
  q_simple_mediation(x="x", m="m1", y="y", cov="c1", data=data,
                     fit_method="sem", missing="mi", parallel=FALSE,
                     progress=FALSE, ...)
}

test_that("raw data are imputed internally, separately from the path model", {
  d <- mi_raw_data()
  q <- mi_quick(d, mi_vars="z", mi_seed=44,
                mice_args=list(m=3, maxit=2), R=200, seed=12)
  expect_s4_class(get_fit(q), "lavaan.mi")
  expect_s3_class(q$imputation, "mids")
  expect_equal(q$imputation$m, 3)
  expect_setequal(names(q$imputation$data), c("x", "m1", "y", "c1", "z"))
  expect_false("z" %in% lavaan::lavNames(get_fit(q), "ov"))
  expect_equal(q$mi_info$used, 3)
  expect_identical(q$ci_type, "mc")
  expect_true(all(is.finite(confint(q$ind_out$ustd))))
  expect_output(print(q, pvalue=FALSE), "Pooled path estimates")
  expect_error(plot(q), "not yet available for MI")
  q2 <- mi_quick(d, mi_vars="z", mi_seed=44,
                 mice_args=list(m=3, maxit=2), R=200, seed=12)
  expect_equal(q$imputed_data, q2$imputed_data)
  expect_equal(confint(q$ind_out$stdxy), confint(q2$ind_out$stdxy))
})

test_that("completed data and mids match explicit sem.mi and manual pooling", {
  d <- mi_raw_data()
  imp <- mice::mice(d[, c("x", "m1", "y", "c1", "z")], m=3,
                     maxit=2, seed=18, printFlag=FALSE)
  completed <- mice::complete(imp, "all")
  q <- mi_quick(completed, ci_type="mc", R=200, seed=15)
  qm <- mi_quick(imp, ci_type="mc", R=200, seed=15)
  ref <- suppressWarnings(do.call(lavaan.mi::sem.mi,
                   list(model="m1 ~ x + c1\ny ~ m1 + x + c1",
                        data=completed, fixed.x=TRUE, meanstructure=TRUE)))
  a <- mean(vapply(ref@ParTableList, function(pt) pt$est[pt$lhs=="m1" &
                                                   pt$op=="~" & pt$rhs=="x"], numeric(1)))
  b <- mean(vapply(ref@ParTableList, function(pt) pt$est[pt$lhs=="y" &
                                                   pt$op=="~" & pt$rhs=="m1"], numeric(1)))
  expect_equal(as.numeric(coef(q$ind_out$ustd)), a*b, tolerance=1e-7)
  expect_equal(confint(q$ind_out$ustd), confint(qm$ind_out$ustd))
  for (type in names(q$ind_out)) {
    r <- indirect_effect(x="x", m="m1", y="y", fit=ref, ci_type="mc",
              standardized_x=type %in% c("stdx", "stdxy"),
              standardized_y=type %in% c("stdy", "stdxy"),
              R=200, seed=15, parallel=FALSE, progress=FALSE)
    expect_equal(as.numeric(coef(q$ind_out[[type]])), as.numeric(coef(r)), tolerance=1e-6)
    expect_equal(as.numeric(confint(q$ind_out[[type]])), as.numeric(confint(r)), tolerance=1e-6)
  }
  expect_identical(qm$imputation, imp)
})

test_that("all model forms forward the MI settings", {
  d <- mi_raw_data()
  imp <- mice::mice(d[, c("x", "m1", "m2", "y", "c1")],
                    m=3, maxit=2, seed=3, printFlag=FALSE)
  models <- list(parallel="m1 ~ x + c1\nm2 ~ x + c1\ny ~ m1 + m2 + x + c1",
                 serial="m1 ~ x + c1\nm2 ~ m1 + x + c1\ny ~ m1 + m2 + x + c1")
  for (form in names(models)) {
    fun <- switch(form, parallel=q_parallel_mediation, serial=q_serial_mediation)
    q <- fun(x="x", m=c("m1","m2"), y="y", cov="c1", data=imp,
              fit_method="sem", missing="mi", boot_ci=FALSE, mc_ci=FALSE,
              progress=FALSE)
    ref <- suppressWarnings(do.call(lavaan.mi::sem.mi,
                list(model=models[[form]], data=mice::complete(imp,"all"),
                     fixed.x=TRUE, meanstructure=TRUE)))
    coef_mi <- methods::getMethod("coef", "lavaan.mi", where=asNamespace("lavaan.mi"))
    actual <- coef_mi(get_fit(q)); expected <- coef_mi(ref)
    expect_equal(actual[order(names(actual))], expected[order(names(expected))], tolerance=1e-6)
    expect_null(q$ci_type)
  }
  q <- q_mediation(x="x", y="y", model=c("x -> m1 -> y", "x -> m2 -> y"),
                    cov="c1", data=imp, fit_method="sem", missing="mi",
                    boot_ci=FALSE, mc_ci=FALSE, progress=FALSE)
  expect_s4_class(get_fit(q), "lavaan.mi")
  expect_equal(length(q$ind_out$ustd), 2)
})

test_that("unsupported combinations and incomplete imputations fail clearly", {
  d <- mi_raw_data()
  expect_error(mi_quick(d, ci_type="boot"), "not bootstrap")
  expect_error(mi_quick(d, boot_ci=TRUE), "not bootstrap")
  expect_error(mi_quick(d, moderators=list("x -> y"="z")), "without indicators")
  expect_error(mi_quick(d, indicators=list(m1=c("m1","m2"))), "without indicators")
  expect_error(mi_quick(d, mi_vars="absent"), "all model/mi_vars")
  expect_error(mi_quick(d, mice_args=list(data=d)), "not mice_args")
  expect_error(mi_quick(d, mice_args=list(3)), "named list")
  expect_error(mi_quick(d, sem_args=list(3)), "named list")
  expect_error(mi_quick(d, mi_seed=1, mice_args=list(seed=2)), "only one")
  expect_error(mi_quick(list(d,d)), "complete, finite numeric")
  expect_error(mi_quick(list(d)), "at least two")
  expect_error(mi_quick(list(d,d), mice_args=list(m=3)), "only to raw")
  expect_error(mi_quick(d, sem_args=list(group="z")), "Unsupported sem_args")
  expect_error(mi_quick(d, sem_args=list(estimator="WLSMV")), "only ML and MLR")
  expect_error(q_simple_mediation(x="x",m="m1",y="y",data=d,missing="mi"),
                "require observed-variable SEM")
  expect_error(q_simple_mediation(x="x",m="m1",y="y",data=d,
                                  mice_args=list(m=3)), "require missing")
})
