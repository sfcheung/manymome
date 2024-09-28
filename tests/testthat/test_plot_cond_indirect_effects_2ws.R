# To be examined in an interactive session.

library(testthat)
library(manymome)

suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1

lm_m3 <- lm(m3 ~ m1 + x*gp*w4, dat)
lm_y <- lm(y ~ m2 + m3*w4*gp, dat)
fit_lm <- lm2list(lm_m3, lm_y)

out_mm_1 <- mod_levels_list("w4", "gp",
                            sd_from_mean = c(-1, 1),
                            fit = fit_lm, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "m3", y = "y", fit = fit_lm)

test_that("Two moderators", {
  expect_no_error(plot(out_1))
  expect_no_error(plot(out_1, facet_grid_rows = "gp"))
  expect_no_error(plot(out_1, facet_grid_cols = "gp", digits = 3))
  expect_no_error(plot(out_1, facet_grid_rows = "w4"))
  expect_no_error(plot(out_1, facet_grid_cols = "w4", digits = 2))

  expect_no_error(plot(out_1, graph_type = "tumble"))
  expect_no_error(plot(out_1, facet_grid_rows = "gp", graph_type = "tumble"))
  expect_no_error(plot(out_1, facet_grid_cols = "gp", graph_type = "tumble"))
  expect_no_error(plot(out_1, facet_grid_rows = "w4", graph_type = "tumble"))
  expect_no_error(plot(out_1, facet_grid_cols = "w4", graph_type = "tumble"))
})

lm_m3 <- lm(m3 ~ m1, dat)
lm_y <- lm(y ~ m2 + m3*w4*gp*city, dat)
fit_lm <- lm2list(lm_m3, lm_y)

out_mm_1 <- mod_levels_list("w4", "gp", "city",
                            sd_from_mean = c(-1, 1),
                            fit = fit_lm, merge = TRUE)

out_1 <- cond_indirect_effects(wlevels = out_mm_1, x = "m3", y = "y", fit = fit_lm)

test_that("Three moderators", {
  expect_no_error(plot(out_1))
  expect_no_error(plot(out_1, facet_grid_rows = "gp", facet_grid_cols = "city"))
  expect_no_error(plot(out_1, facet_grid_rows = "gp", facet_grid_cols = "w4", digits = 1))
  expect_no_error(plot(out_1, facet_grid_rows = "w4", facet_grid_cols = "city"))

  expect_no_error(plot(out_1, graph_type = "tumble"))
  expect_no_error(plot(out_1, graph_type = "tumble", facet_grid_rows = "gp", facet_grid_cols = "city"))
  expect_no_error(plot(out_1, graph_type = "tumble", facet_grid_rows = "gp", facet_grid_cols = "w4"))
  expect_no_error(plot(out_1, graph_type = "tumble", facet_grid_rows = "w4", facet_grid_cols = "city"))
})
