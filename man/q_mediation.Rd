% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/q_mediation.R
\name{q_mediation}
\alias{q_mediation}
\alias{q_simple_mediation}
\alias{q_serial_mediation}
\alias{q_parallel_mediation}
\alias{print.q_mediation}
\title{Mediation Models By Regression
or SEM}
\usage{
q_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  model = NULL,
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

q_simple_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

q_serial_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

q_parallel_mediation(
  x,
  y,
  m = NULL,
  cov = NULL,
  data = NULL,
  boot_ci = TRUE,
  mc_ci = FALSE,
  level = 0.95,
  R = 100,
  seed = NULL,
  ci_type = NULL,
  boot_type = c("perc", "bc"),
  fit_method = c("lm", "regression", "sem", "lavaan"),
  missing = "fiml",
  fixed.x = TRUE,
  sem_args = list(),
  na.action = "na.pass",
  parallel = TRUE,
  ncores = max(parallel::detectCores(logical = FALSE) - 1, 1),
  progress = TRUE
)

\method{print}{q_mediation}(
  x,
  digits = 4,
  annotation = TRUE,
  pvalue = TRUE,
  pvalue_digits = 4,
  se = TRUE,
  for_each_path = FALSE,
  se_ci = TRUE,
  wrap_computation = TRUE,
  lm_ci = TRUE,
  lm_beta = TRUE,
  lm_ci_level = 0.95,
  sem_style = c("lm", "lavaan"),
  ...
)
}
\arguments{
\item{x}{For \code{\link[=q_mediation]{q_mediation()}},
\code{\link[=q_simple_mediation]{q_simple_mediation()}},
\code{\link[=q_serial_mediation]{q_serial_mediation()}}, and
\code{\link[=q_parallel_mediation]{q_parallel_mediation()}}, it is the
name of the predictor. For the
\code{print} method of these
functions, \code{x} is the output of these
functions.}

\item{y}{The name of the outcome.}

\item{m}{A character vector of the
name(s) of the mediator(s). For
a simple mediation model, it must
has only one name. For serial and
parallel mediation models, it can
have one or more names. For a serial
mediation models, the direction of
the paths go from the first names to
the last names. For example,
\code{c("m1", "m3", "m4")} denoted that
the path is \code{m1 -> m3 -> m4}.}

\item{cov}{The names of the covariates,
if any. If it is a character vector,
then the outcome (\code{y}) and all
mediators (\code{m}) regress on all
the covariates. If it is a named
list of character vectors, then the
covariates in an element predict
only the variable with the name of this
element. For example, \code{list(m1 = "c1", dv = c("c2", "c3"))}
indicates that \code{c1} predicts \code{"m1"},
while \code{c2} and \code{c3} predicts \code{"dv"}.
Default is \code{NULL}, no covariates.}

\item{data}{The data frame. Note that
listwise deletion will be used and
only cases with no missing data on
all variables in the model (e.g.,
\code{x}, \code{m}, \code{y} and \code{cov}) will be
retained.}

\item{boot_ci}{Logical. Whether
bootstrap confidence interval will be
formed. Default is \code{TRUE}.}

\item{mc_ci}{Logical. Whether
Monte Carlo confidence interval will be
formed. Default is \code{FALSE}. Only
supported if \code{fit_method} is
\code{"sem"} or \code{"lavaan"}.}

\item{level}{The level of confidence
of the confidence interval. Default
is .95 (for 95\% confidence intervals).}

\item{R}{The number of bootstrap
samples. Default is 100. Should be
set to 5000 or at least 10000.}

\item{seed}{The seed for the random
number generator. Default is \code{NULL}.
Should nearly always be set to an
integer to make the results
reproducible.}

\item{ci_type}{The type of
confidence intervals to be formed.
Can be either \code{"boot"} (bootstrapping)
or \code{"mc"} (Monte Carlo). If not
supplied or is \code{NULL}, will check
other arguments
(e.g, \code{boot_ci} and \code{mc_ci}). If
supplied, will override \code{boot_ci}
and \code{mc_ci}. If \code{fit_method} is
\code{"regression"} or \code{"lm"}, then only
\code{"boot"} is supported.}

\item{boot_type}{The type of the
bootstrap confidence intervals.
Default is \code{"perc"}, percentile
confidence interval. Set \code{"bc"} for
bias-corrected confidence interval.
Ignored if \code{ci_type} is not \code{"boot"}.}

\item{model}{The type of model. For
\code{\link[=q_mediation]{q_mediation()}}, it can be
\code{"simple"} (simple mediation model),
\code{"serial"} (serial mediation model),
or \code{"parallel"} (parallel mediation
model). It is recommended to call
the corresponding wrappers directly
(\code{\link[=q_simple_mediation]{q_simple_mediation()}},
\code{\link[=q_serial_mediation]{q_serial_mediation()}}, and
\code{\link[=q_parallel_mediation]{q_parallel_mediation()}}) instead of
call \code{\link[=q_mediation]{q_mediation()}}.}

\item{fit_method}{How the model is
to be fitted. If set to \code{"lm"} or
\code{"regression"},
linear regression will be used
(fitted by \code{\link[stats:lm]{stats::lm()}}). If set
to \code{"sem"} or \code{"lavaan"}, structural
equation modeling will be used and
the model will be fitted by
\code{\link[lavaan:sem]{lavaan::sem()}}. Default is \code{"lm"}.}

\item{missing}{If \code{fit_method} is
set to \code{"sem"} or \code{"lavaan"}, this
argument determine how missing data
is handled. The default value is
\code{"fiml"} and full information maximum
likelihood will be used to handle
missing data. Please refer to
\link[lavaan:lavOptions]{lavaan::lavOptions} for other options.}

\item{fixed.x}{If \code{fit_method} is
set to \code{"sem"} or \code{"lavaan"}, this
determines whether the observed
predictors ("x" variables, including
control variables) are treated as
fixed variables or random variables.
Default is \code{TRUE}, to mimic the
same implicit setting in
regression fitted by \code{\link[stats:lm]{stats::lm()}}.}

\item{sem_args}{If \code{fit_method} is
set to \code{"sem"} or \code{"lavaan"}, this
is a named list of arguments to be
passed to \code{\link[lavaan:sem]{lavaan::sem()}}. Arguments
listed here will not override
\code{missing} and \code{fixed.x}.}

\item{na.action}{How missing data is
handled. Used only when \code{fit_method}
is set to \code{"sem"} or \code{"lavaan"}. If
\code{"na.pass"}, the default, then all
data will be passed to \code{lavaan}, and
full information maximum likelihood
(\code{fiml}) will be used to handle
missing data. If \code{"na.omit"}, then
listwise deletion will be used.}

\item{parallel}{If \code{TRUE}, default,
parallel processing will be used when
doing bootstrapping.}

\item{ncores}{Integer. The number of
CPU cores to use when \code{parallel} is
\code{TRUE}. Default is the number of
non-logical cores minus one (one
minimum). Will raise an error if
greater than the number of cores
detected by
\code{\link[parallel:detectCores]{parallel::detectCores()}}. If
\code{ncores} is set, it will override
\code{make_cluster_args} in \code{\link[=do_boot]{do_boot()}}.}

\item{progress}{Logical. Display
progress or not.}

\item{digits}{Number of digits to
display. Default is 4.}

\item{annotation}{Logical. Whether
the annotation after the table of
effects is to be printed. Default is
\code{TRUE.}}

\item{pvalue}{Logical. If \code{TRUE},
asymmetric \emph{p}-values based on
bootstrapping will be printed if
available. Default is \code{TRUE}.}

\item{pvalue_digits}{Number of decimal
places to display for the \emph{p}-values.
Default is 4.}

\item{se}{Logical. If \code{TRUE} and
confidence intervals are available,
the standard errors of the estimates
are also printed. They are simply the
standard deviations of the bootstrap
estimates. Default is \code{TRUE}.}

\item{for_each_path}{Logical. If
\code{TRUE}, each of the paths will be
printed individually, using the
\code{print}-method of the output of
\code{\link[=indirect_effect]{indirect_effect()}}. Default is
\code{FALSE}.}

\item{se_ci}{Logical. If \code{TRUE} and
confidence interval has not been
computed, the function will try
to compute them from stored
standard error if the original
standard error is to be used.
Ignored
if confidence interval has already
been computed. Default
is \code{TRUE}.}

\item{wrap_computation}{Logical.
If \code{TRUE}, the default, long
computational symbols and values
will be wrapped to fit to the screen
width.}

\item{lm_ci}{If \code{TRUE},
when printing the regression results
of \code{\link[stats:lm]{stats::lm()}},
confidence
interval based on \emph{t} statistic
and standard error will be computed
and added to the output. Default is
\code{TRUE}.}

\item{lm_beta}{If \code{TRUE},
when printing the regression results
of \code{\link[stats:lm]{stats::lm()}},
standardized coefficients are
computed and included in the
printout. Only numeric variables will
be computed, and any derived terms,
such as product terms, will be formed
\emph{after} standardization. Default
is \code{TRUE}.}

\item{lm_ci_level}{The level of confidence
of the confidence interval. Ignored
if \code{lm_ci} is not \code{TRUE}.}

\item{sem_style}{How the for the
model is to be printed if the model
is fitted by structural equation
modeling (using \code{lavaan}). Default
is \code{"lm"} and the results will be
printed in a style similar to that
of \code{\link[=summary]{summary()}} output of \code{\link[stats:lm]{stats::lm()}}.
If \code{"lavaan"}, the results will be
printed in usual \code{lavaan} style.}

\item{...}{Other arguments. If
\code{for_each_path} is \code{TRUE}, they
will be passed to the print method
of the output of \code{\link[=indirect_effect]{indirect_effect()}}.
Ignored otherwise.}
}
\value{
The function \code{\link[=q_mediation]{q_mediation()}} returns
a \code{q_mediation} class object, with
its \code{print} method.

The function \code{\link[=q_simple_mediation]{q_simple_mediation()}} returns
a \code{q_simple_mediation} class object, which
is a subclass of \code{q_mediation}.

The function \code{\link[=q_serial_mediation]{q_serial_mediation()}} returns
a \code{q_serial_mediation} class object, which
is a subclass of \code{q_mediation}.

The function \code{\link[=q_parallel_mediation]{q_parallel_mediation()}} returns
a \code{q_parallel_mediation} class object, which
is a subclass of \code{q_mediation}.
}
\description{
Simple-to-use functions
for fitting linear models by regression
or structural equation modeling and
testing indirect effects, using
just one function.
}
\details{
The family of "q" (quick) functions
are for testing mediation effects in
common models. These functions do the
following in one single call:
\itemize{
\item Fit the linear models.
\item Compute and test all the indirect
effects.
}

They are easy-to-use and are suitable
for common models with mediators.
For now, there are
"q" functions for these models:
\itemize{
\item A simple mediation: One predictor
(\code{x}), one mediator (\code{m}), one
outcome (\code{y}), and optionally some
control variables (\code{cov}ariates)
(\code{\link[=q_simple_mediation]{q_simple_mediation()}})
\item A serial mediation model: One
predictor (\code{x}), one or more
mediators (\code{m}), one outcome (\code{y}),
and optionally some control variables
(\code{cov}ariates). The mediators
positioned sequentially between \code{x}
and \code{y} (\code{\link[=q_serial_mediation]{q_serial_mediation()}}):
\itemize{
\item \code{x -> m1 -> m2 -> ... -> y}
}
\item A parallel mediation model: One
predictor (\code{x}), one or more
mediators (\code{m}), one outcome (\code{y}),
and optionally some control variables
(\code{cov}ariates). The mediators
positioned in parallel between \code{x}
and \code{y} (\code{\link[=q_parallel_mediation]{q_parallel_mediation()}}):
\itemize{
\item \code{x -> m1 -> y}
\item \code{x -> m2 -> y}
\item ...
}
\item An arbitrary mediation model: One
predictor (\code{x}), one or more
mediators (\code{m}), one outcome (\code{y}),
and optionally some control variables
(\code{cov}ariates). The mediators
positioned in an arbitrary form
between \code{x}
and \code{y}, as long as there are no
feedback loops (\code{\link[=q_mediation]{q_mediation()}}).
For example:
\itemize{
\item \code{x -> m1}
\item \code{m1 -> m21 -> y}
\item \code{m1 -> m22 -> y}
\item ...
}
}

Users only need to specify the \code{x},
\code{m}, and \code{y} variables, and covariates
or control variables, if any (by \code{cov}),
and the functions will automatically
identify all indirect effects and
total effects.

Note that they are \emph{not} intended to
be flexible. For more complex models,
it is recommended to fit the models
manually, either by structural
equation modelling (e.g.,
\code{\link[lavaan:sem]{lavaan::sem()}}) or by regression
analysis using \code{\link[stats:lm]{stats::lm()}} or
\code{\link[lmhelprs:many_lm]{lmhelprs::many_lm()}}. See
\url{https://sfcheung.github.io/manymome/articles/med_lm.html}
for an illustration on how to compute
and test indirect effects for an
arbitrary mediation model.
\subsection{Specifying a Model of an Arbitrary Form}{

If a custom model is to be estimated,
instead of setting \code{model} to a name
of the form (\code{"simple"},\code{"serial"},
or \code{"parallel"}), set \verb{model to the paths between }x\code{and}y`. It can
take one of the following two forms:

A character vector, each element a
string of a path, with variable names
connected by \code{"->"} (the spaces are
optional):

\preformatted{c("x -> m11 -> m12 -> y",
  "x -> m2 -> y")}

A list of character vectors, each
vector is a vector of names representing
a path, going from the first element
to the last element:

\preformatted{list(c("x", "m11, "m12", "y"),
     c("x", "m2", "y")}

The two forms above specify the same
model.

Paths not included are fixed to zero
(i.e., does not "exist" in the model).
A path can be specified more than once
if this can enhance readability.
For example:

\preformatted{c("x1 -> m1 -> m21 -> y1",
  "x1 -> m1 -> m22 -> y1")}

The path \code{"x1 -> m1"} appears twice,
to indicate two different pathways from
\code{x1} to \code{y1}.
}

\subsection{Workflow}{

The coefficients of the model can be
estimated by one of these two
methods: OLS (ordinary least squares)
regression (setting \code{fit_method} to
\code{"regression"} or \code{"lm"}), or path
analysis (SEM, structural equation
modeling, by setting \code{fit_method} to
\code{"sem"} or \code{"lavaan"}).
\subsection{Regression}{

This is the workflow of the "q"
functions when estimating the
coefficients by regression:
\itemize{
\item Do listwise deletion based on all
the variables used in the models.
\item Generate the regression models
based on the variables specified.
\item Fit all the models by OLS regression
using \code{\link[stats:lm]{stats::lm()}}.
\item Call \code{\link[=all_indirect_paths]{all_indirect_paths()}} to
identify all indirect paths.
\item Call \code{\link[=many_indirect_effects]{many_indirect_effects()}} to
compute all indirect effects and
form their confidence intervals.
\item Call \code{\link[=total_indirect_effect]{total_indirect_effect()}} to
compute the total indirect effect.
\item Return all the results for printing.
}

The output of the "q" functions have
a \code{print} method for
printing all the major results.
}

\subsection{Path Analysis}{

This is the workflow of the "q"
functions when estimating the
coefficients by path analysis (SEM):
\itemize{
\item By default, cases with missing
data only on the mediators and the
outcome variable will be retained,
and full information maximum
likelihood (FIML) will be used to
estimate the coefficients.
(Controlled by \code{missing}, default
to \code{"fiml"}) using \code{\link[lavaan:sem]{lavaan::sem()}}.
\item Generate the SEM (\code{lavaan}) model
syntax based on the model specified.
\item Fit the model by path analysis
using \code{\link[lavaan:sem]{lavaan::sem()}}.
\item Call \code{\link[=all_indirect_paths]{all_indirect_paths()}} to
identify all indirect paths.
\item Call \code{\link[=many_indirect_effects]{many_indirect_effects()}} to
compute all indirect effects and
form their confidence intervals.
\item Call \code{\link[=total_indirect_effect]{total_indirect_effect()}} to
compute the total indirect effect.
\item Return all the results for printing.
}
}

}

\subsection{Testing the Indirect Effects}{

Two methods are available for testing
the indirect effects: nonparametric
bootstrap confidence intervals
(\code{ci_type} set to \code{"boot"}) and
Monte Carlo confidence intervals
(\code{ci_type} set to \code{"mc"}).

If the coefficients are estimated by
OLS regression, only nonparametric
bootstrap confidence intervals are
supported.

If the coefficients are estimated by
path analysis (SEM), then both methods
are supported.
}

\subsection{Printing the Results}{

The output of the "q" functions have
a \code{print} method for
printing all the major results.
}

\subsection{Notes}{
\subsection{Flexibility}{

The "q" functions are designed to be
easy to use. They are not designed to
be flexible. For maximum flexibility,
fit the models manually and call
functions such as
\code{\link[=indirect_effect]{indirect_effect()}} separately. See
\url{https://sfcheung.github.io/manymome/articles/med_lm.html}
for illustrations.
}

\subsection{Monte Carlo Confidence Intervals}{

We do not recommend using Monte Carlo
confidence intervals for models
fitted by regression because the
covariances between parameter
estimates are assumed to be zero,
which may not be the case in some
models. Therefore, the "q" functions
do not support Monte Carlo confidence
intervals if OLS regression is used.
}

}
}
\section{Methods (by generic)}{
\itemize{
\item \code{print(q_mediation)}: The \code{print} method of the outputs
of \code{\link[=q_mediation]{q_mediation()}}, \code{\link[=q_simple_mediation]{q_simple_mediation()}},
\code{\link[=q_serial_mediation]{q_serial_mediation()}}, and \code{\link[=q_parallel_mediation]{q_parallel_mediation()}}.

}}
\section{Functions}{
\itemize{
\item \code{q_mediation()}: The general
"q" function for common mediation
models. Not to be used directly.

\item \code{q_simple_mediation()}: A wrapper of \code{\link[=q_mediation]{q_mediation()}} for
simple mediation models (a model with only one mediator).

\item \code{q_serial_mediation()}: A wrapper of \code{\link[=q_mediation]{q_mediation()}} for
serial mediation models.

\item \code{q_parallel_mediation()}: A wrapper of \code{\link[=q_mediation]{q_mediation()}} for
parallel mediation models.

}}
\examples{

# ===== A user-specified mediation model

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

out <- q_mediation(x = "x1",
                   y = "y1",
                   model = c("x1 -> m11 -> m2 -> y1",
                             "x1 -> m12 -> m2 -> y1"),
                   cov = c("c2", "c1"),
                   data = data_med_complicated,
                   R = 40,
                   seed = 1234,
                   parallel = FALSE,
                   progress = FALSE)
# Suppressed printing of p-values due to the small R
# Remove `pvalue = FALSE` when R is large
print(out,
      pvalue = FALSE)


# ===== Simple mediation

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

out <- q_simple_mediation(x = "x",
                          y = "y",
                          m = "m",
                          cov = c("c2", "c1"),
                          data = data_med,
                          R = 20,
                          seed = 1234,
                          parallel = FALSE,
                          progress = FALSE)
# Suppressed printing of p-values due to the small R
# Remove `pvalue = FALSE` when R is large
print(out,
      pvalue = FALSE)

# # Different control variables for m and y
# out <- q_simple_mediation(x = "x",
#                           y = "y",
#                           m = "m",
#                           cov = list(m = "c1",
#                                      y = c("c1", "c2")),
#                           data = data_med,
#                           R = 100,
#                           seed = 1234,
#                           parallel = FALSE,
#                           progress = FALSE)
# out


# ===== Serial mediation

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

# out <- q_serial_mediation(x = "x",
#                           y = "y",
#                           m = c("m1", "m2"),
#                           cov = c("c2", "c1"),
#                           data = data_serial,
#                           R = 40,
#                           seed = 1234,
#                           parallel = FALSE,
#                           progress = FALSE)

# # Suppressed printing of p-values due to the small R
# # Remove `pvalue = FALSE` when R is large
# print(out,
#       pvalue = FALSE)

# # Different control variables for m and y
# out <- q_serial_mediation(x = "x",
#                           y = "y",
#                           m = c("m1", "m2"),
#                           cov = list(m1 = "c1",
#                                      m2 = c("c2", "c1"),
#                                      y = "c2"),
#                           data = data_serial,
#                           R = 100,
#                           seed = 1234,
#                           parallel = FALSE,
#                           progress = FALSE)
# out


# ===== Parallel mediation

# Set R to 5000 or 10000 in real studies
# Remove 'parallel' or set it to TRUE for faster bootstrapping
# Remove 'progress' or set it to TRUE to see a progress bar

# out <- q_parallel_mediation(x = "x",
#                             y = "y",
#                             m = c("m1", "m2"),
#                             cov = c("c2", "c1"),
#                             data = data_parallel,
#                             R = 40,
#                             seed = 1234,
#                             parallel = FALSE,
#                             progress = FALSE)
# # Suppressed printing of p-values due to the small R
# # Remove `pvalue = FALSE` when R is large
# print(out,
#       pvalue = FALSE)

# # Different control variables for m and y
# out <- q_parallel_mediation(x = "x",
#                             y = "y",
#                             m = c("m1", "m2"),
#                             cov = list(m1 = "c1",
#                                        m2 = c("c2", "c1"),
#                                        y = "c2"),
#                             data = data_parallel,
#                             R = 100,
#                             seed = 1234,
#                             parallel = FALSE,
#                             progress = FALSE)
# out

}
\seealso{
\code{\link[lmhelprs:many_lm]{lmhelprs::many_lm()}} for
fitting several regression models
using model syntax,
\code{\link[=indirect_effect]{indirect_effect()}} for computing and
testing a specific path,
\code{\link[=all_indirect_paths]{all_indirect_paths()}} for
identifying all paths in a model,
\code{\link[=many_indirect_effects]{many_indirect_effects()}} for
computing and testing indirect
effects along several paths, and
\code{\link[=total_indirect_effect]{total_indirect_effect()}} for
computing and testing the total
indirect effects.
}
