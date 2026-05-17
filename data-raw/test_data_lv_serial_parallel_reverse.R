# Generate data
library(lavaan)
n <- 600
mod_pop <-
"
fx =~ .8*x_1 + .8*x_2 + .8*x_3 + (-.8)*x_4
fm1 =~ .9*m1_1 + .8*m1_2 + .9*m1_3 + .8*m1_4
fm2 =~ .6*m2_1 + .6*m2_2 + .6*m2_3 + .6*m2_4
fm3 =~ .8*m3_1 + .8*m3_2 + .8*m3_3 + .8*m3_4
fy =~ .8*y_1 + .8*y_2 + .8*y_3 + (-.8)*y_4
fc1 =~ .8*c1_1 + .8*c1_2 + .8*c1_3 + .8*c1_4
fc2 =~ .8*c2_1 + .8*c2_2 + .8*c2_3 + .8*c2_4
fx ~~ .4 * fc1
fx ~~ .3 * fc2
fm1 ~ .7 * fx
fm3 ~ .7 * fm1
fm2 ~ .59 * fx
fy ~ .7 * fm3 + .57 * fm2 + .4 * fc1 + .2 * fc2
"
set.seed(1357)
dat <- round(simulateData(mod_pop, sample.nobs = n), 2)
# Scale scores
dat$x <- rowMeans(cbind(dat[, c("x_1", "x_2", "x_3")], -dat$x_4))
dat$m1 <- rowMeans(dat[, c("m1_1", "m1_2", "m1_3", "m1_4")])
dat$m2 <- rowMeans(dat[, c("m2_1", "m2_2", "m2_3", "m2_4")])
dat$m3 <- rowMeans(dat[, c("m3_1", "m3_2", "m3_3", "m3_4")])
dat$y <- rowMeans(cbind(dat[, c("y_1", "y_2", "y_3")], -dat$y_4))
dat$c1 <- rowMeans(dat[, c("c1_1", "c1_2", "c1_3", "c1_4")])
dat$c2 <- rowMeans(dat[, c("c2_1", "c2_2", "c2_3", "c2_4")])

head(dat)

# ==== Full ====

mod <-
"
fx =~ x_1 + x_2 + x_3 + x_4
fm1 =~ m1_1 + m1_2 + m1_3 + m1_4
fm2 =~ m2_1 + m2_2 + m2_3 + m2_4
fm3 =~ m3_1 + m3_2 + m3_3 + m3_4
fy =~ y_1 + y_2 + y_3 + y_4
fc1 =~ c1_1 + c1_2 + c1_3 + c1_4
fc2 =~ c2_1 + c2_2 + c2_3 + c2_4
fm1 ~ a1 * fx + fc1 + fc2
fm3 ~ b1 * fm1 + fc1 + fc2
fm2 ~ a2 * fx + fc1 + fc2
fy ~ b3 * fm3 + b2 * fm2 + fc1 + fc2
ab13 := a1 * b1 * b3
ab2 := a2 * b2
"
fit <- sem(mod, dat)
fit
parameterEstimates(fit, standardized = TRUE)

mod_score <-
"
m1 ~ a1 * x + c1 + c2
m3 ~ b1 * m1 + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b3 * m3 + b2 * m2 + x + c1 + c2
ab13 := a1 * b1 * b3
ab2 := a2 * b2
"
fit_score <- sem(mod_score, dat)
fit_score
parameterEstimates(fit_score, standardized = TRUE)

# ==== Simple ====

mod_simple <-
"
fx =~ x_1 + x_2 + x_3 + x_4
fm1 =~ m1_1 + m1_2 + m1_3 + m1_4
fy =~ y_1 + y_2 + y_3 + y_4
fc1 =~ c1_1 + c1_2 + c1_3 + c1_4
fc2 =~ c2_1 + c2_2 + c2_3 + c2_4
fm1 ~ a * fx + fc1 + fc2
fy ~ b* fm1 + fx + fc1 + fc2
ab := a*b
"
fit_simple <- sem(mod_simple, dat)
fit_simple
parameterEstimates(fit1, standardized = TRUE)

mod_simple_score <-
"
m1 ~ a * x + c1 + c2
y ~ b * m1 + x + c1 + c2
ab := a*b
"
fit_simple_score <- sem(mod_simple_score, dat)
fit_simple_score
parameterEstimates(fit_simple_score, standardized = TRUE)

# ==== Serial ====

mod_serial <-
"
fx =~ x_1 + x_2 + x_3 + x_4
fm1 =~ m1_1 + m1_2 + m1_3 + m1_4
fm3 =~ m3_1 + m3_2 + m3_3 + m3_4
fy =~ y_1 + y_2 + y_3 + y_4
fc1 =~ c1_1 + c1_2 + c1_3 + c1_4
fc2 =~ c2_1 + c2_2 + c2_3 + c2_4
fm1 ~ a1 * fx + fc1 + fc2
fm3 ~ b1 * fm1 + fc1 + fc2
fy ~ b3 * fm3 + fc1 + fc2
ab13 := a1 * b1 * b3
"
fit_serial <- sem(mod_serial, dat)
fit_serial
parameterEstimates(fit_serial, standardized = TRUE)

mod_serial_score <-
"
m1 ~ a1 * x + c1 + c2
m3 ~ b1 * m1 + c1 + c2
y ~ b3 * m3 + x + c1 + c2
ab13 := a1 * b1 * b3
"
fit_serial_score <- sem(mod_serial_score, dat)
fit_serial_score
parameterEstimates(fit_serial_score, standardized = TRUE)

# ==== Parallel ====

mod_parallel <-
"
fx =~ x_1 + x_2 + x_3 + x_4
fm1 =~ m1_1 + m1_2 + m1_3 + m1_4
fm2 =~ m2_1 + m2_2 + m2_3 + m2_4
fy =~ y_1 + y_2 + y_3 + y_4
fc1 =~ c1_1 + c1_2 + c1_3 + c1_4
fc2 =~ c2_1 + c2_2 + c2_3 + c2_4
fm1 ~ a1 * fx + fc1 + fc2
fm2 ~ a2 * fx + fc1 + fc2
fy ~ b1 * fm1 + b2 * fm2 + fc1 + fc2
ab1 := a1 * b1
ab2 := a2 * b2
ab := ab1 + ab2
"
fit_parallel <- sem(mod_parallel, dat)
fit_parallel
parameterEstimates(fit_parallel, standardized = TRUE)

mod_parallel_score <-
"
m1 ~ a1 * x + c1 + c2
m2 ~ a2 * x + c1 + c2
y ~ b1 * m1 + b2 * m2 + x + c1 + c2
ab1 := a1 * b1
ab2 := a2 * b2
ab := ab1 + ab2
"
fit_parallel_score <- sem(mod_parallel_score, dat)
fit_parallel_score
parameterEstimates(fit_parallel_score, standardized = TRUE)

# ==== Save the dataset ====

data_indicators <- dat
usethis::use_data(data_indicators, overwrite = TRUE)

