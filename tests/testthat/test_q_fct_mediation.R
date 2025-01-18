skip_on_cran()

library(testthat)
library(manymome)

# Test: Simple mediation

test_that("q function: simple mediation", {
out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 200,
                          seed = 1234)
out1 <- q_simple_mediation(x = "x",
                           y = "y",
                           m = "m",
                           cov = list(y = c("c2", "c1"),
                                      m = "c2"),
                           data = data_med,
                           R = 200,
                           seed = 1234,
                           boot_type = "bc")
lm_m <- lm(m ~ x + c2 + c1, data = data_med)
lm_m_c2 <- lm(m ~ x + c2, data = data_med)
lm_y <- lm(y ~ m + x + c2 + c1, data = data_med)
lm_all <- lm2list(lm_m, lm_y)
lm_all2 <- lm2list(lm_m_c2, lm_y)
chk0 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = lm_all,
                        boot_ci = TRUE,
                        R = 200,
                        seed = 1234,
                        parallel = FALSE,
                        progress = FALSE)
chk1 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = lm_all,
                        boot_ci = TRUE,
                        boot_out = chk0,
                        R = 200,
                        seed = 1234,
                        standardized_x = TRUE,
                        standardized_y = TRUE,
                        parallel = FALSE,
                        progress = FALSE)
chk2 <- indirect_effect(x = "x",
                        y = "y",
                        m = "m",
                        fit = lm_all2,
                        boot_ci = TRUE,
                        boot_type = "bc",
                        R = 200,
                        seed = 1234,
                        standardized_y = TRUE,
                        parallel = FALSE,
                        progress = FALSE)
expect_equal(coef(out$ind_out$ustd),
             coef(chk0),
             ignore_attr = TRUE)
expect_equal(confint(out$ind_out$ustd),
             confint(chk0),
             ignore_attr = TRUE)
expect_equal(coef(out$ind_out$stdxy),
             coef(chk1),
             ignore_attr = TRUE)
expect_equal(confint(out$ind_out$stdxy),
             confint(chk1),
             ignore_attr = TRUE)
expect_equal(confint(out1$ind_out$stdy),
             confint(chk2),
             ignore_attr = TRUE)
expect_error(q_simple_mediation(x = "x",
                                y = "y",
                                m = "m1",
                                cov = c("c2", "c1"),
                                data = data_med,
                                R = 100,
                                seed = 1234))
})

# Test: Serial mediation

test_that("q function: serial mediation", {
out0 <- q_serial_mediation(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           cov = c("c2", "c1"),
                           data = data_serial,
                           R = 100,
                           seed = 1234)
out1 <- q_serial_mediation(x = "x",
                           y = "y",
                           m = c("m1", "m2"),
                           cov = list(y = c("c2", "c1"),
                                      m1 = "c2",
                                      m2 = "c1"),
                           data = data_serial,
                           R = 100,
                           seed = 1234)
lm_m1 <- lm(m1 ~ x + c2 + c1, data = data_serial)
lm_m2 <- lm(m2 ~ m1 + x + c1 + c2, data = data_serial)
lm_m1_v1 <- lm(m1 ~ x + c2, data = data_serial)
lm_m2_v1 <- lm(m2 ~ m1 + x + c1, data = data_serial)
lm_y <- lm(y ~ m1 + m2 + x + c2 + c1, data = data_serial)
lm_all0 <- lm2list(lm_m1, lm_m2, lm_y)
lm_all1 <- lm2list(lm_m1_v1, lm_m2_v1, lm_y)
chk0 <- indirect_effect(x = "x",
                        y = "y",
                        m = c("m1", "m2"),
                        fit = lm_all0,
                        boot_ci = TRUE,
                        R = 100,
                        seed = 1234,
                        parallel = FALSE,
                        progress = FALSE)
chk1 <- indirect_effect(x = "x",
                        y = "y",
                        m = c("m1", "m2"),
                        fit = lm_all0,
                        boot_ci = TRUE,
                        boot_out = chk0,
                        R = 100,
                        seed = 1234,
                        standardized_x = TRUE,
                        standardized_y = TRUE,
                        parallel = FALSE,
                        progress = FALSE)
chk2 <- indirect_effect(x = "x",
                        y = "y",
                        m = c("m1", "m2"),
                        fit = lm_all1,
                        boot_ci = TRUE,
                        R = 100,
                        seed = 1234,
                        standardized_y = TRUE,
                        parallel = FALSE,
                        progress = FALSE)
expect_equal(coef(out0$ind_out$ustd[[1]]),
             coef(chk0),
             ignore_attr = TRUE)
expect_equal(confint(out0$ind_out$ustd[[1]]),
             confint(chk0),
             ignore_attr = TRUE)
expect_equal(coef(out0$ind_out$stdxy[[1]]),
             coef(chk1),
             ignore_attr = TRUE)
expect_equal(confint(out0$ind_out$stdxy[[1]]),
             confint(chk1),
             ignore_attr = TRUE)
expect_equal(confint(out1$ind_out$stdy[[1]]),
             confint(chk2),
             ignore_attr = TRUE)
expect_error(q_serial_mediation(x = "x",
                                y = "y",
                                m = "m",
                                cov = c("c2", "c1"),
                                data = data_serial,
                                R = 100,
                                seed = 1234))
})

# Test: Parallel mediation

test_that("q function: parallel mediation", {
out0 <- q_parallel_mediation(x = "x",
                              y = "y",
                              m = c("m1", "m2"),
                              cov = c("c2", "c1"),
                              data = data_parallel,
                              R = 100,
                              seed = 1234)
out1 <- q_parallel_mediation(x = "x",
                             y = "y",
                             m = c("m1", "m2"),
                             cov = list(y = c("c2", "c1"),
                                        m1 = "c2",
                                        m2 = "c1"),
                             data = data_parallel,
                             R = 100,
                             seed = 1234)
lm_m1 <- lm(m1 ~ x + c2 + c1, data = data_parallel)
lm_m2 <- lm(m2 ~ x + c1 + c2, data = data_parallel)
lm_m1_v1 <- lm(m1 ~ x + c2, data = data_parallel)
lm_m2_v1 <- lm(m2 ~ x + c1, data = data_parallel)
lm_y <- lm(y ~ m1 + m2 + x + c2 + c1, data = data_parallel)
lm_all0 <- lm2list(lm_m1, lm_m2, lm_y)
lm_all1 <- lm2list(lm_m1_v1, lm_m2_v1, lm_y)
chk0a <- indirect_effect(x = "x",
                         y = "y",
                         m = "m1",
                         fit = lm_all0,
                         boot_ci = TRUE,
                         R = 100,
                         seed = 1234,
                         parallel = FALSE,
                         progress = FALSE)
chk0b <- indirect_effect(x = "x",
                         y = "y",
                         m = "m2",
                         fit = lm_all0,
                         boot_ci = TRUE,
                         boot_out = chk0a,
                         R = 100,
                         seed = 1234,
                         parallel = FALSE,
                         progress = FALSE)
chk1a <- indirect_effect(x = "x",
                         y = "y",
                         m = "m1",
                         fit = lm_all0,
                         boot_ci = TRUE,
                         boot_out = chk0a,
                         R = 100,
                         seed = 1234,
                         standardized_x = TRUE,
                         standardized_y = TRUE,
                         parallel = FALSE,
                         progress = FALSE)
chk1b <- indirect_effect(x = "x",
                         y = "y",
                         m = "m2",
                         fit = lm_all0,
                         boot_ci = TRUE,
                         boot_out = chk0a,
                         R = 100,
                         seed = 1234,
                         standardized_x = TRUE,
                         standardized_y = TRUE,
                         parallel = FALSE,
                         progress = FALSE)
chk2a <- indirect_effect(x = "x",
                         y = "y",
                         m = "m1",
                         fit = lm_all1,
                         boot_ci = TRUE,
                         R = 100,
                         seed = 1234,
                         standardized_y = TRUE,
                         parallel = FALSE,
                         progress = FALSE)
chk2b <- indirect_effect(x = "x",
                         y = "y",
                         m = "m2",
                         fit = lm_all1,
                         boot_ci = TRUE,
                         boot_out = chk2a,
                         R = 100,
                         seed = 1234,
                         standardized_y = TRUE,
                         parallel = FALSE,
                         progress = FALSE)

expect_equal(coef(out0$ind_out$ustd[[1]]),
             coef(chk0a),
             ignore_attr = TRUE)
expect_equal(coef(out0$ind_out$ustd[[2]]),
             coef(chk0b),
             ignore_attr = TRUE)
expect_equal(confint(out0$ind_total$ustd),
             confint(chk0a + chk0b),
             ignore_attr = TRUE)

expect_equal(coef(out0$ind_out$stdxy[[1]]),
             coef(chk1a),
             ignore_attr = TRUE)
expect_equal(coef(out0$ind_out$stdxy[[2]]),
             coef(chk1b),
             ignore_attr = TRUE)
expect_equal(confint(out0$ind_out$stdxy[[1]]),
             confint(chk1a),
             ignore_attr = TRUE)
expect_equal(confint(out0$ind_out$stdxy[[2]]),
             confint(chk1b),
             ignore_attr = TRUE)
expect_equal(confint(out0$ind_total$stdxy),
             confint(chk1a + chk1b),
             ignore_attr = TRUE)

expect_equal(coef(out1$ind_out$stdy[[1]]),
             coef(chk2a),
             ignore_attr = TRUE)
expect_equal(coef(out1$ind_out$stdy[[2]]),
             coef(chk2b),
             ignore_attr = TRUE)
expect_equal(confint(out1$ind_out$stdy[[1]]),
             confint(chk2a),
             ignore_attr = TRUE)
expect_equal(confint(out1$ind_out$stdy[[2]]),
             confint(chk2b),
             ignore_attr = TRUE)

expect_equal(confint(out1$ind_total$stdy),
             confint(chk2a + chk2b),
             ignore_attr = TRUE)
expect_error(q_parallel_mediation(x = "x",
                                y = "y",
                                m = "m",
                                cov = c("c2", "c1"),
                                data = data_parallel,
                                R = 100,
                                seed = 1234))
})

