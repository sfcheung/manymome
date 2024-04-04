skip("WIP")

#' @title Pseudo Johnson-Neyman Probing
#'
#' @description One paragraph description
#'
#' @details Details
#'   (Include subjects for verbs.)
#'   (Use 3rd person forms for verbs.)
#'
#' @return
#' Specify what are returned.
#'
#' @param arg1 Argument description.
#' @param ... Additional arguments.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. Advance online publication. *Health Psychology*.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [functionname()]
#'
#' @family relatedfunctions
#'
#' @examples
#' \donttest{
#' }
#'
#' @export
#'
#' @describeIn topic Description of this function
#' @order 1
tmpfct <- function(x, y = c("a", "b", "c"), d = NA) {
  }

library(lavaan)
library(manymome)
library(ggplot2)
dat <- data_med_mod_a
dat$wx <- dat$x * dat$w
mod <-
"
m ~ x  + w + wx
y  ~ m + x
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Create levels of w1, the moderators
wlevels <- mod_levels("w", fit = fit, sd_from_mean = c(-5, 0, 5))
wlevels

# Conditional effects from x to y when w1 is equal to each of the levels
boot_out <- do_boot(fit,
                    R = 50,
                    seed = 4314,
                    parallel = FALSE,
                    progress = FALSE)
out <- cond_indirect_effects(x = "x", y = "y", m = "m",
                             wlevels = wlevels,
                             fit = fit,
                             boot_ci = TRUE,
                             boot_out = boot_out)
out
confint(out)

tmpfct <- function(w0, which = c("lower", "upper"),
                   type = c("distance", "limit", "ci", "est")) {
    which <- match.arg(which)
    type <- match.arg(type)
    out <- cond_indirect(wvalues = c(w = w0),
                        x = "x", y = "y", m = "m",
                        fit = fit,
                        boot_ci = TRUE, boot_out = boot_out)
    out1 <- (switch(which, lower = confint(out)[2],
                           upper = confint(out)[1]))
    return(switch(type,
                  distance = out1^2,
                  limit = out1,
                  ci = confint(out),
                  est = coef(out)))
  }
tmpfct(-3)
tmpfct(2)
tmpfct(3)


w1_bound <- mod_levels(w = "w", fit = fit, sd_from_mean = c(-5, 5))
w_lower <- optim(min(wlevels),
              fn = function(x) tmpfct(x, which = "lower"), method = "Brent",
              lower = min(wlevels), upper = max(wlevels))
w_upper <- optim(max(wlevels),
              fn = function(x) tmpfct(x, which = "upper"), method = "Brent",
              lower = min(wlevels), upper = max(wlevels))
w_lower$par
w_upper$par
w1_bound

cond_indirect_effects(x = "x", y = "y", m = "m",
                      wlevels = mod_levels("w",
                                           values = c(w_lower$par, w_upper$par),
                                           fit = fit),
                      fit = fit,
                      boot_ci = TRUE, boot_out = boot_out)

wi <- seq(from = min(wlevels) - 6, to = max(wlevels) + 6, length.out = 100)
tmplower <- sapply(wi, function(x) tmpfct(x, which = "lower", type = "distance"))
plot(wi, tmplower)
abline(h = 0)
tmpupper <- sapply(wi, function(x) tmpfct(x, which = "upper", type = "distance"))
plot(wi, tmpupper)
abline(h = 0)

tmpci <- sapply(wi, function(x) tmpfct(x, type = "ci"))
tmpest <- sapply(wi, function(x) tmpfct(x, type = "est"))
ylim <- c(min(tmpci), max(tmpci))
plot(wi, tmpci[1, ], ylim = ylim, type = "l", lty = "dotted",
     ylab = "Conditional Indirect Effect",
     xlab = "Moderator")
points(wi, tmpci[2, ], type = "l", lty = "dotted")
points(wi, tmpest, type = "l", lwd = 2)
abline(h = 0, lwd = 1)

wi <- seq(from = min(wlevels) - 6, to = max(wlevels) + 6, length.out = 100)
wlevels_long <- mod_levels(w = "w", fit = fit, values = wi)
tmp <- cond_indirect_effects(wlevels = wlevels_long,
                             x = "x", y = "y", m = "m",
                             fit = fit,
                             boot_ci = TRUE, boot_out = boot_out)
plot_effect_vs_w(tmp)
tmpdat <- data.frame(w = attr(tmp, "wlevels")$w,
                     ci.lower = confint(tmp)[, 1],
                     ci.upper = confint(tmp)[, 2],
                     effect = coef(tmp))
p <- ggplot(tmpdat) +
     geom_line(aes(x = w, y = ci.lower), linetype = "dotted", size = 1) +
     geom_line(aes(x = w, y = ci.upper), linetype = "dotted", size = 1) +
     geom_line(aes(x = w, y = effect), linetype = "solid", size = 1) +
     geom_hline(yintercept = 0, linetype = "solid", color = "red", size = .5) +
     theme(panel.background = element_rect(fill = "lightgrey"))
plot(p)

tmpfct <- function(w0, which = c("lower", "upper")) {
    out <- cond_indirect(wvalues = c(w = w0),
                         x = "x", y = "y", m = "m",
                         fit = fit,
                         boot_ci = TRUE, boot_out = boot_out)
    switch(which, lower = confint(out)[2],
                  upper = confint(out)[1])^2
  }
w_lower <- optim(min(wlevels_long),
              fn = function(x) tmpfct(x, which = "lower"), method = "Brent",
              lower = min(wlevels_long), upper = max(wlevels_long))
w_upper <- optim(max(wlevels_long),
              fn = function(x) tmpfct(x, which = "upper"), method = "Brent",
              lower = min(wlevels_long), upper = max(wlevels_long))
w_lower$par
w_upper$par

# If not in the range

tmpfct <- function(w0, which = c("lower", "upper")) {
    out <- cond_indirect(wvalues = c(w = w0),
                         x = "x", y = "y", m = "m",
                         fit = fit,
                         boot_ci = TRUE, boot_out = boot_out)
    switch(which, lower = confint(out)[2],
                  upper = confint(out)[1])^2
  }
w_lower <- optimize(function(x) tmpfct(x, which = "lower"),
              lower = min(wlevels_long) + 3, upper = max(wlevels_long))
w_upper <- optimize(function(x) tmpfct(x, which = "upper"),
              lower = min(wlevels_long) + 3, upper = max(wlevels_long))
if (isTRUE(all.equal(w_lower$objective, 0))) w_lower$minimum else NA
if (isTRUE(all.equal(w_upper$objective, 0))) w_upper$minimum else NA