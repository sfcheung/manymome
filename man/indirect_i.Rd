% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indirect.R
\name{indirect_i}
\alias{indirect_i}
\title{Indirect Effect (No
Bootstrapping)}
\usage{
indirect_i(
  x,
  y,
  m = NULL,
  fit = NULL,
  est = NULL,
  implied_stats = NULL,
  wvalues = NULL,
  standardized_x = FALSE,
  standardized_y = FALSE,
  computation_digits = 5,
  prods = NULL,
  get_prods_only = FALSE,
  data = NULL,
  expand = TRUE,
  warn = TRUE,
  allow_mixing_lav_and_obs = TRUE,
  group = NULL,
  est_vcov = NULL,
  df_residual = NULL
)
}
\arguments{
\item{x}{Character. The name of the
predictor at the start of the path.}

\item{y}{Character. The name of the
outcome variable at the end of the
path.}

\item{m}{A vector of the variable
names of the mediator(s). The path
goes from the first mediator
successively to the last mediator. If
\code{NULL}, the default, the path goes
from \code{x} to \code{y}.}

\item{fit}{The fit object. Currently
only supports \link[lavaan:lavaan-class]{lavaan::lavaan}
objects. Support for lists of \code{\link[=lm]{lm()}}
output is implemented by high level
functions such as \code{\link[=indirect_effect]{indirect_effect()}}
and \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.
It can also be
a \code{lavaan.mi} object
returned by
\code{\link[lavaan.mi:lavaan.mi]{lavaan.mi::lavaan.mi()}} or
its wrapper, such as \code{\link[lavaan.mi:lavaan.mi]{lavaan.mi::sem.mi()}}.}

\item{est}{The output of
\code{\link[lavaan:parameterEstimates]{lavaan::parameterEstimates()}}. If
\code{NULL}, the default, it will be
generated from \code{fit}. If supplied,
\code{fit} will be ignored.}

\item{implied_stats}{Implied means,
variances, and covariances of
observed variables and latent
variables (if any), of the form of
the output of \code{\link[lavaan:lavInspect]{lavaan::lavInspect()}}
with \code{what} set to \code{"implied"}, but
with means extracted with \code{what} set
to \code{"mean.ov"} and \code{"mean.lv"}. The
standard deviations are extracted
from this object for standardization.
Default is \code{NULL}, and implied
statistics will be computed from
\code{fit} if required.}

\item{wvalues}{A numeric vector of
named elements. The names are the
variable names of the moderators, and
the values are the values to which
the moderators will be set to.
Default is \code{NULL}.}

\item{standardized_x}{Logical.
Whether \code{x} will be standardized.
Default is \code{FALSE}.}

\item{standardized_y}{Logical.
Whether \code{y} will be standardized.
Default is \code{FALSE}.}

\item{computation_digits}{The number
of digits in storing the computation
in text. Default is 3.}

\item{prods}{The product terms found.
For internal use.}

\item{get_prods_only}{IF \code{TRUE}, will
quit early and return the product
terms found. The results can be
passed to the \code{prod} argument when
calling this function. Default is
\code{FALSE}. For internal use.}

\item{data}{Data frame (optional). If
supplied, it will be used to identify
the product terms. For internal use.}

\item{expand}{Whether products of
more than two terms will be searched.
\code{TRUE} by default. For internal use.}

\item{warn}{If \code{TRUE}, the default,
the function will warn against
possible misspecification, such as
not setting the value of a moderator
which moderate one of the component
path. Set this to \code{FALSE} will
suppress these warnings. Suppress
them only when the moderators are
omitted intentionally.}

\item{allow_mixing_lav_and_obs}{If
\code{TRUE}, it accepts a path with both
latent variables and observed
variables. Default is \code{TRUE}.}

\item{group}{Either the group number
as appeared in the \code{\link[=summary]{summary()}}
or \code{\link[lavaan:parameterEstimates]{lavaan::parameterEstimates()}}
output of an \code{lavaan}-class object,
or the group label as used in
the \code{lavaan}-class object.
Used only when the number of
groups is greater than one. Default
is NULL.}

\item{est_vcov}{A list of
variance-covariance matrix of
estimates, one for each response
variable (\code{y}-variable). Used only
for models fitted by \code{\link[stats:lm]{stats::lm()}}
for now. It is used to compute the
standard error for a path with no
mediator, and both \code{x} and \code{y} are
not standardized.}

\item{df_residual}{A numeric
vector of the residual degrees of
freedom for the model of each
response variable (\code{y}-variable).
Used only for models fitted by
\code{\link[stats:lm]{stats::lm()}} for now. It is used to
compute the \emph{p}-value and confidence
interval for a path with no mediator
and both \code{x} and \code{y} are not
standardized.}
}
\value{
It returns an
\code{indirect}-class object. This class
has the following methods:
\code{\link[=coef.indirect]{coef.indirect()}},
\code{\link[=print.indirect]{print.indirect()}}. The
\code{\link[=confint.indirect]{confint.indirect()}} method is used
only when called by \code{\link[=cond_indirect]{cond_indirect()}}
or \code{\link[=cond_indirect_effects]{cond_indirect_effects()}}.
}
\description{
It computes an indirect
effect, optionally conditional on the
value(s) of moderator(s) if present.
}
\details{
This function is a low-level
function called by
\code{\link[=indirect_effect]{indirect_effect()}},
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}, and
\code{\link[=cond_indirect]{cond_indirect()}}, which call this
function multiple times if bootstrap
confidence interval is requested.

This function usually should not be
used directly. It is exported for
advanced users and developers
}
\examples{

library(lavaan)
dat <- modmed_x1m3w4y1
mod <-
"
m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
"
fit <- sem(mod, dat, meanstructure = TRUE,
           fixed.x = FALSE, se = "none", baseline = FALSE)
est <- parameterEstimates(fit)

wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)

# Compute the conditional indirect effect by indirect_i()
indirect_1 <- indirect_i(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
                       wvalues = wvalues)

# Manually compute the conditional indirect effect
indirect_2 <- (est[est$label == "a1", "est"] +
                wvalues["w1"] * est[est$label == "d1", "est"]) *
              (est[est$label == "a2", "est"] +
                wvalues["w2"] * est[est$label == "d2", "est"]) *
              (est[est$label == "a3", "est"] +
                wvalues["w3"] * est[est$label == "d3", "est"]) *
              (est[est$label == "a4", "est"] +
                wvalues["w4"] * est[est$label == "d4", "est"])
# They should be the same
coef(indirect_1)
indirect_2

}
\seealso{
\code{\link[=indirect_effect]{indirect_effect()}},
\code{\link[=cond_indirect_effects]{cond_indirect_effects()}}, and
\code{\link[=cond_indirect]{cond_indirect()}}, the high level
functions that should usually be
used.
}
