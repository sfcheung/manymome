skip("To be run in an interactive section")
library(testthat)
library(manymome)
suppressMessages(library(lavaan))
suppressMessages(library(semTools))
suppressMessages(library(mice))

dat <- data_med
dat[1, 1] <- dat[2, 3] <- dat[3, 5] <- dat[4, 3] <- dat[5, 2] <- NA
head(dat)

library(mice)
set.seed(26245)
out_mice <- mice(dat, m = 5, printFlag = FALSE)
dat_mi <- complete(out_mice, action = "all")

mod <-
"
m ~ x + c1 + c2
y ~ m + x + c1 + c2
"
fit_lav <- sem(model = mod,
               data = dat,
               missing = "fiml.x")
summary(fit_lav)
fit_mi <- sem.mi(model = mod,
                 data = dat_mi)
summary(fit_mi)

system.time(mc_out_lav <- do_mc(fit = fit_lav,
                                   R = 5000,
                                   seed = 4234))
system.time(mc_out_mi <- do_mc(fit = fit_mi,
                                   R = 5000,
                                   seed = 4234))
