
library(manymome)
suppressMessages(library(lavaan))
dat <- modmed_x1m3w4y1
n <- nrow(dat)
set.seed(860314)
dat$gp <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
dat$city <- sample(c("alpha", "beta", "gamma", "sigma"), n, replace = TRUE)
lm_m1 <- lm(m1 ~ x * w1, dat)
lm_m2 <- lm(m2 ~ m1 + gp + city, dat)
lm_m3 <- lm(m3 ~ m1 + x * gp, dat)
lm_y <- lm(y ~ m2 + m3 + x * w4, dat)
lm_m1_mm <- model.matrix(lm_m1)[, 4]
lm_m2_mm <- model.matrix(lm_m2)[, -c(1:2)]
lm_m3_mm <- model.matrix(lm_m3)[, 6:7]
lm_y_mm <- model.matrix(lm_y)[, 6]
dat2 <- cbind(dat, lm_m1_mm, lm_m2_mm, lm_m3_mm, lm_y_mm)
fit <- list(lm_m1, lm_m2, lm_m3, lm_y)

# mod_levels(fit, w = list(gp = c("gpgp2", "gpgp3"),
#                          w1 = "w1"))
# Output:
# gp:
# data.frame(label = c("cat1", "cat2"),
#            gpgp2 = c(1, 0),
#            gpgp3 = c(0, 1))
# w1:
# data.frame(label = c("Low", "Medium", "High"),
#            w1 = c(-2, 0, 2))

out_n_sd <- mod_levels(fit, w = "w1")
out_n_p <- mod_levels(fit, w = "w4", w_method = "percentile")
out_n_sd2 <- mod_levels(fit, w = "w1", sd_from_mean = c(-1, 4))
out_n_p2 <- mod_levels(fit, w = "w1", w_method = "percentile", percentiles = c(.25, .75))

out_c <- mod_levels(fit, w = c("gpgp2", "gpgp3"), w_type = "categorical")
out_c2 <- mod_levels(fit, w = c("citybeta", "citygamma", "citysigma"), w_type = "categorical")
out_c3 <- mod_levels(fit, w = c("gpgp2", "gpgp3"), prefix = "gp", w_type = "categorical")

out_m_1 <- merge_mod_levels(out_n_sd, out_n_p)
out_m_2 <- merge_mod_levels(out_n_sd, out_c)
out_m_3 <- merge_mod_levels(out_n_sd, out_c2)
out_m_4 <- merge_mod_levels(out_c2, out_n_p2)
out_m_5 <- merge_mod_levels(out_c, out_c2)
attr(out_m_5, "wlevels")

out_m_l_2 <- merge_mod_levels(mod_levels_list("w1", c("gpgp2", "gpgp3"), fit = fit))
out_m_l_m_2 <- mod_levels_list("w1", c("gpgp2", "gpgp3"), fit = fit, merge = TRUE)

out_mm_1 <- merge_mod_levels(mod_levels(fit, w = "w1"),
                             mod_levels(fit, w = c("gpgp2", "gpgp3"), w_type = "categorical"),
                             mod_levels(fit, w = "w4", w_method = "percentile"))

# tmp1 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "m1", fit = fit)
# tmp1_tmp <- sapply(tmp1, function(x) data.frame(indirect = x$indirect, raw = x$indirect_raw), simplify = FALSE)
# do.call(rbind, tmp1_tmp)

# tmp1s <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "m1", fit = fit, standardized_x = TRUE, standardized_y = TRUE)
# tmp1s_tmp <- sapply(tmp1s, function(x) data.frame(indirect = x$indirect, raw = x$indirect_raw), simplify = FALSE)
# do.call(rbind, tmp1s_tmp)

# tmp1s <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "m1", fit = fit,
#                                standardized_x = TRUE, standardized_y = TRUE, boot_ci = TRUE,
#                                seed = 54154)
# tmp1s_tmp <- sapply(tmp1s, function(x) data.frame(indirect = x$indirect, raw = x$indirect_raw,
#                                                   boot.cl.lower = x$boot_ci[1],
#                                                   boot.cl.upper = x$boot_ci[2]), simplify = FALSE)
# do.call(rbind, tmp1s_tmp)


# tmp2 <- cond_indirect_effects(wlevels = out_mm_1, x = "x", y = "m2", , m = "m1", fit = fit)
# data.frame(sapply(tmp2, function(x) x$indirect))
# split(out_mm_1, seq_len(nrow(out_mm_1)))
# Known "answers"

out_m_1_chk <- structure(list(w1 = c(1.22805757313838, 1.22805757313838, 1.22805757313838,
0.258999906944399, 0.258999906944399, 0.258999906944399, -0.71005775924958,
-0.71005775924958, -0.71005775924958), w4 = c(1.20145409352787,
0.0429229585375893, -0.856078089831277, 1.20145409352787, 0.0429229585375893,
-0.856078089831277, 1.20145409352787, 0.0429229585375893, -0.856078089831277
)), row.names = c("w1: M+1.0SD; w4: 84%", "w1: M+1.0SD; w4: 50%",
"w1: M+1.0SD; w4: 16%", "w1: Mean; w4: 84%", "w1: Mean; w4: 50%",
"w1: Mean; w4: 16%", "w1: M-1.0SD; w4: 84%", "w1: M-1.0SD; w4: 50%",
"w1: M-1.0SD; w4: 16%"), class = "data.frame", wlevels = structure(list(
    w1 = c("M+1.0SD", "M+1.0SD", "M+1.0SD", "Mean", "Mean", "Mean",
    "M-1.0SD", "M-1.0SD", "M-1.0SD"), w4 = c("84%", "50%", "16%",
    "84%", "50%", "16%", "84%", "50%", "16%")), class = "data.frame", row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9")), wvars = list(w1 = "w1",
    w4 = "w4"), w_types = c(w1 = "numeric", w4 = "numeric"))

out_m_2_chk <- structure(list(w1 = c(1.22805757313838, 1.22805757313838, 1.22805757313838,
0.258999906944399, 0.258999906944399, 0.258999906944399, -0.71005775924958,
-0.71005775924958, -0.71005775924958), gpgp2 = c(0, 1, 0, 0,
1, 0, 0, 1, 0), gpgp3 = c(0, 0, 1, 0, 0, 1, 0, 0, 1)), row.names = c("w1: M+1.0SD; gp: gp1",
"w1: M+1.0SD; gp: gp2", "w1: M+1.0SD; gp: gp3", "w1: Mean; gp: gp1",
"w1: Mean; gp: gp2", "w1: Mean; gp: gp3", "w1: M-1.0SD; gp: gp1",
"w1: M-1.0SD; gp: gp2", "w1: M-1.0SD; gp: gp3"), class = "data.frame", wlevels = structure(list(
    w1 = c("M+1.0SD", "M+1.0SD", "M+1.0SD", "Mean", "Mean", "Mean",
    "M-1.0SD", "M-1.0SD", "M-1.0SD"), gp = c("gp1", "gp2", "gp3",
    "gp1", "gp2", "gp3", "gp1", "gp2", "gp3")), class = "data.frame", row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9")), wvars = list(w1 = "w1",
    gp = c("gpgp2", "gpgp3")), w_types = c(w1 = "numeric", gp = "categorical"
))

out_m_3_chk <- structure(list(w1 = c(1.22805757313838, 1.22805757313838, 1.22805757313838,
1.22805757313838, 0.258999906944399, 0.258999906944399, 0.258999906944399,
0.258999906944399, -0.71005775924958, -0.71005775924958, -0.71005775924958,
-0.71005775924958), citybeta = c(0, 1, 0, 0, 0, 1, 0, 0, 0, 1,
0, 0), citygamma = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0), citysigma = c(0,
0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1)), row.names = c("w1: M+1.0SD; city: alpha",
"w1: M+1.0SD; city: beta", "w1: M+1.0SD; city: gamma", "w1: M+1.0SD; city: sigma",
"w1: Mean; city: alpha", "w1: Mean; city: beta", "w1: Mean; city: gamma",
"w1: Mean; city: sigma", "w1: M-1.0SD; city: alpha", "w1: M-1.0SD; city: beta",
"w1: M-1.0SD; city: gamma", "w1: M-1.0SD; city: sigma"), class = "data.frame", wlevels = structure(list(
    w1 = c("M+1.0SD", "M+1.0SD", "M+1.0SD", "M+1.0SD", "Mean",
    "Mean", "Mean", "Mean", "M-1.0SD", "M-1.0SD", "M-1.0SD",
    "M-1.0SD"), city = c("alpha", "beta", "gamma", "sigma", "alpha",
    "beta", "gamma", "sigma", "alpha", "beta", "gamma", "sigma"
    )), class = "data.frame", row.names = c("1", "2", "3", "4",
"5", "6", "7", "8", "9", "10", "11", "12")), wvars = list(w1 = "w1",
    city = c("citybeta", "citygamma", "citysigma")), w_types = c(w1 = "numeric",
city = "categorical"))

out_m_4_chk <- structure(list(citybeta = c(0, 0, 1, 1, 0, 0, 0, 0), citygamma = c(0,
0, 0, 0, 1, 1, 0, 0), citysigma = c(0, 0, 0, 0, 0, 0, 1, 1),
    w1 = c(0.850514535665991, -0.380019362976597, 0.850514535665991,
    -0.380019362976597, 0.850514535665991, -0.380019362976597,
    0.850514535665991, -0.380019362976597)), row.names = c("city: alpha; w1: 75%",
"city: alpha; w1: 25%", "city: beta; w1: 75%", "city: beta; w1: 25%",
"city: gamma; w1: 75%", "city: gamma; w1: 25%", "city: sigma; w1: 75%",
"city: sigma; w1: 25%"), class = "data.frame", wlevels = structure(list(
    city = c("alpha", "alpha", "beta", "beta", "gamma", "gamma",
    "sigma", "sigma"), w1 = c("75%", "25%", "75%", "25%", "75%",
    "25%", "75%", "25%")), class = "data.frame", row.names = c("1",
"2", "3", "4", "5", "6", "7", "8")), wvars = list(city = c("citybeta",
"citygamma", "citysigma"), w1 = "w1"), w_types = c(city = "categorical",
w1 = "numeric"))

out_m_5_chk <- structure(list(gpgp2 = c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0),
    gpgp3 = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1), citybeta = c(0,
    1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0), citygamma = c(0, 0, 1,
    0, 0, 0, 1, 0, 0, 0, 1, 0), citysigma = c(0, 0, 0, 1, 0,
    0, 0, 1, 0, 0, 0, 1)), row.names = c("gp: gp1; city: alpha",
"gp: gp1; city: beta", "gp: gp1; city: gamma", "gp: gp1; city: sigma",
"gp: gp2; city: alpha", "gp: gp2; city: beta", "gp: gp2; city: gamma",
"gp: gp2; city: sigma", "gp: gp3; city: alpha", "gp: gp3; city: beta",
"gp: gp3; city: gamma", "gp: gp3; city: sigma"), class = "data.frame", wlevels = structure(list(
    gp = c("gp1", "gp1", "gp1", "gp1", "gp2", "gp2", "gp2", "gp2",
    "gp3", "gp3", "gp3", "gp3"), city = c("alpha", "beta", "gamma",
    "sigma", "alpha", "beta", "gamma", "sigma", "alpha", "beta",
    "gamma", "sigma")), class = "data.frame", row.names = c("1",
"2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")), wvars = list(
    gp = c("gpgp2", "gpgp3"), city = c("citybeta", "citygamma",
    "citysigma")), w_types = c(gp = "categorical", city = "categorical"
))

test_that("merge_mod_levels", {
    expect_equal(out_m_1, out_m_1_chk, ignore_attr = TRUE)
    expect_equal(out_m_2, out_m_2_chk, ignore_attr = TRUE)
    expect_equal(out_m_3, out_m_3_chk, ignore_attr = TRUE)
    expect_equal(out_m_4, out_m_4_chk, ignore_attr = TRUE)
    expect_equal(out_m_5, out_m_5_chk, ignore_attr = TRUE)
    expect_equal(out_m_l_2, out_m_l_m_2, ignore_attr = TRUE)
  })