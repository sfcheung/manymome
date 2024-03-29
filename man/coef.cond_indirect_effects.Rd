% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef_cond_indirect_effects.R
\name{coef.cond_indirect_effects}
\alias{coef.cond_indirect_effects}
\title{Estimates of Conditional
Indirect Effects or Conditional
Effects}
\usage{
\method{coef}{cond_indirect_effects}(object, ...)
}
\arguments{
\item{object}{The output of
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.}

\item{...}{Optional arguments.
Ignored by the function.}
}
\value{
A numeric vector: The
estimates of the conditional effects
or conditional indirect effects.
}
\description{
Return the estimates of
the conditional indirect effects or
conditional effects for all levels in
the output of
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.
}
\details{
It extracts and returns the
column \code{ind} or \code{std} in the output
of \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.
}
\examples{

library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ x  + w1 + x:w1
m2 ~ m1
y  ~ m2 + x + w4 + m2:w4
"
fit <- sem(mod, dat,
           meanstructure = TRUE, fixed.x = FALSE,
           se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

# Conditional effects from x to m1 when w1 is equal to each of the levels
out1 <- cond_indirect_effects(x = "x", y = "m1",
                      wlevels = c("w1"), fit = fit)
out1
coef(out1)

# Conditional indirect effects from x1 through m1 and m2 to y,
out2 <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                      wlevels = c("w1", "w4"), fit = fit)
out2
coef(out2)

# Standardized conditional indirect effects from x1 through m1 and m2 to y,
out2std <- cond_indirect_effects(x = "x", y = "y", m = c("m1", "m2"),
                      wlevels = c("w1", "w4"), fit = fit,
                      standardized_x = TRUE, standardized_y = TRUE)
out2std
coef(out2std)

}
\seealso{
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}
}
