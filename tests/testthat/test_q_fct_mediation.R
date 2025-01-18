skip("WIP")

library(testthat)
library(manymome)

# Test: Simple mediation

out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 500,
                          seed = 1234)
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
summary(out$lm_out[["m"]])
summary(out$lm_out[["y"]])
out$ind_total[[1]]

out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = list(m = "c2",
                                     y = c("c1", "c2")),
                          data = data_med,
                          R = 500,
                          seed = 1234,
                          boot_type = c("bc"))

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
out$ind_total[[1]]

# Test: Serial mediation

out <- q_serial_mediation(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          cov = c("c2", "c1"),
                          data = data_serial,
                          R = 500,
                          seed = 1234)

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]

out <- q_serial_mediation(x = "x",
                          y = "y",
                          m = c("m1", "m2"),
                          cov = list(m1 = "c1",
                                     m2 = c("c2", "c1"),
                                     y = "c2"),
                          data = data_serial,
                          R = 500,
                          seed = 1234)

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
summary(out$lm_out[[3]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]

out <- q_parallel_mediation(x = "x",
                            y = "y",
                            m = c("m1", "m2"),
                            cov = c("c2", "c1"),
                            data = data_parallel,
                            R = 500,
                            seed = 1234)

summary(out$lm_out[[1]])
summary(out$lm_out[[2]])
summary(out$lm_out[[3]])
out$ind_out[[1]]
out$ind_out[[2]]
out$ind_out[[3]]
out$ind_out[[4]]
out$ind_total[[1]]
out$ind_total[[4]]
