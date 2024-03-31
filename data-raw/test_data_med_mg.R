# Generate data
library(lavaan)
n1 <- 100
n2 <- 100
mod1 <-
"
m11 ~ .4*x1 + .0*x2 + .3*c1 + .1*c2
m12 ~ .5*m11 + .0*x1 + .0*x2 + .0*c1 + .0*c2
m2 ~ .0*x1 + .4*x2 + .1*c1 + .1*c2
y1 ~ .1*m11 + .4*m12 + .0*m2 + .1*x1 + .0*x2 + .0*c1 + .0*c2
y2 ~ .0*m11 + .0*m12 + .4*m2 + .0*x1 + .2*x2 + .0*c1 + .0*c2
"
mod2 <-
"
m11 ~ .2*x1 + .0*x2 + .3*c1 + .1*c2
m12 ~ .5*m11 + .0*x1 + .0*x2 + .0*c1 + .0*c2
m2 ~ .0*x1 + .4*x2 + .1*c1 + .1*c2
y1 ~ .1*m11 + .2*m12 + .0*m2 + .1*x1 + .0*x2 + .0*c1 + .0*c2
y2 ~ .0*m11 + .0*m12 + .4*m2 + .0*x1 + .2*x2 + .0*c1 + .0*c2
"
dat1 <- simulateData(model = mod1, sample.nobs = n1, seed = 1234)
dat2 <- simulateData(model = mod2, sample.nobs = n2, seed = 5678)
head(dat1)
dat1 <- as.data.frame(scale(dat,
        center = c(-10, -5, -5, -5, -8, -5, -10, -5, -10), scale = FALSE))
dat1 <- as.data.frame(scale(dat,
        center = c(-10, -5, -5, -5, -8, -7, -12, -5, -10), scale = FALSE))
mod <-
"
m11 ~ x1 + x2 + c1 + c2
m12 ~ m11 + c1 + c2
m2 ~ x1 + x2 + c1 + c2
y1 ~ m11 + m12 + x1 + x2 + c1 + c2
y2 ~ m2 + x1 + x2 + c1 + c2
"
dat <- rbind(dat1, dat2)
dat$group <- rep(c("Group A", "Group B"), times = c(n1, n2))
fit <- sem(mod, dat, group = "group")
summary(fit, rsquare = TRUE)
data_med_complicated_mg <- dat
usethis::use_data(data_med_complicated_mg, overwrite = TRUE)
