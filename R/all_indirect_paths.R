#' @title Enumerate All Indirect Effects in a Model
#'
#' @description Check all indirect paths in a model and
#' return them as a list of arguments of `x`, `y`,
#' and `m`, to be used by `indirect_effect()`.
#'
#' @details It makes use of [igraph::all_simple_paths()]
#' to identify paths in a model.
#'
#' @return
#' [all_indirect_paths()] returns
#' a list of the class `all_paths`. Each argument is a
#' list of three character vectors,
#' `x`, the name of the predictor that starts a path, `y`,
#' the name of the outcome that ends a path, and `m`, a
#' character vector of one or more names of the mediators,
#' from `x` to `y`. This class has a print method.
#'
#' [all_paths_to_df()] returns a data frame with three
#' columns, `x`, `y`, and `m`, which can be used by
#' functions such as [indirect_effect()].
#'
#' @param fit A fit object. Either the output of
#' [lavaan::lavaan()] or its wrapper such as [lavaan::sem()],
#' or a list of the output of [lm()] or the output of
#' [lm2list()].
#'
#' @param exclude A character vector of variables to be excluded
#' in the search, such as control variables.
#'
#' @param x A character vector of variables that will be
#' included as the `x` variables. If supplied, only
#' paths that start from these variables will be included
#' in the search.
#' If `NULL`, the default, then all variables that are
#' one of the predictors in at least one regression
#' equation will be
#' included in the search.
#'
#' @param y A character vector of variables that will be
#' included as the `y` variables. If supplied, only
#' paths that start from these variables will be included
#' in the search.
#' If `NULL`, the default, then all variables that are
#' the outcome variables in at least one regression
#' equation will be
#' included in the search.
#'
#' @param all_paths An `all_paths`-class object. For example,
#' the output of [all_indirect_paths()].
#'
#' @param group Either the group number
#' as appeared in the [summary()]
#' or [lavaan::parameterEstimates()]
#' output of an `lavaan`-class object,
#' or the group label as used in
#' the `lavaan`-class object.
#' Used only when the number of
#' groups is greater than one. Default
#' is NULL. If not specified by the model
#' has more than one group, than paths
#' that appears in at least one group
#' will be included in the output.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [indirect_effect()], [lm2list()].
#' [many_indirect_effects()]
#'
#'
#' @examples
#' library(lavaan)
#' data(data_serial_parallel)
#' mod <-
#' "
#' m11 ~ x + c1 + c2
#' m12 ~ m11 + x + c1 + c2
#' m2 ~ x + c1 + c2
#' y ~ m12 + m2 + m11 + x + c1 + c2
#' "
#' fit <- sem(mod, data_serial_parallel,
#'            fixed.x = FALSE)
#' # All indirect paths
#' out1 <- all_indirect_paths(fit)
#' out1
#' names(out1)
#'
#' # Exclude c1 and c2 in the search
#' out2 <- all_indirect_paths(fit, exclude = c("c1", "c2"))
#' out2
#' names(out2)
#'
#' # Exclude c1 and c2, and only consider paths start
#' # from x and end at y
#' out3 <- all_indirect_paths(fit, exclude = c("c1", "c2"),
#'                            x = "x",
#'                            y = "y")
#' out3
#' names(out3)
#'
#' @describeIn all_indirect_paths Enumerate all indirect paths.
#'
#' @order 1
#'
#' @export

all_indirect_paths <- function(fit = NULL,
                               exclude = NULL,
                               x = NULL,
                               y = NULL,
                               group = NULL) {
    fit_type <- cond_indirect_check_fit(fit)
    if (is.na(fit_type)) {
        stop("'fit' is not of a supported type.")
      }
    ngroups <- 1
    group_number <- NULL
    group_label <- NULL

    # Create an adjancey matrix
    if (identical(fit_type, "lavaan")) {

        ngroups <- lavaan::lavTech(fit, "ngroups")
        if ((ngroups > 1) && !is.null(group)) {
            group_labels_all <- lavaan::lavTech(fit,
                                                "group.label")
            if (is.numeric(group)) {
                group_label <- group_labels_all[group]
                group_number <- group
              } else {
                group_number <- match(group, group_labels_all)
                group_label <- group
              }
          }
        tmp <- lavaan::lavInspect(fit,
                  drop.list.single.group = FALSE)
        tmp <- lapply(tmp, function(x) x$beta)
        beta <- tmp
      }
    if (identical(fit_type, "lavaan.mi")) {
        # TODO:
        # Add support for multiple group models.
        beta <- list(lavaan::lavInspect(fit)$beta)
      }
    if (identical(fit_type, "lm")) {
        beta <- list(beta_from_lm(fit))
      }
    if ((ngroups > 1) &&
        (identical(fit_type, "lavaan"))) {
        group_labels_all <- lavaan::lavTech(fit,
                                            "group.label")
        if (is.null(group)) {
            groups <- group_labels_all
            group_numbers <- seq_len(ngroups)
          } else {
            beta <- beta[group_number]
            groups <- group
            group_numbers <- group_number
            group_labels_all <- group_labels_all[group_number]
          }
        tmpfct <- function(adj_i,
                           group_i,
                           group_label_i,
                           group_number_i,
                           exclude = exclude,
                           x = x,
                           y = y,
                           fit = fit,
                           fit_type = fit_type) {
                      out <- all_indirect_paths_i(adj = adj_i,
                                                  exclude = exclude,
                                                  x = x,
                                                  y = y,
                                                  fit = fit,
                                                  fit_type = fit_type)
                      for (i in seq_along(out)) {
                          out[[i]]$group_label <- group_label_i
                          out[[i]]$group_number <- group_number_i
                        }
                      out
                    }
        out3 <- mapply(tmpfct,
                       adj_i = beta,
                       group_i = groups,
                       group_label_i = group_labels_all,
                       group_number_i = group_numbers,
                       MoreArgs = list(exclude = exclude,
                                       x = x,
                                       y = y,
                                       fit = fit,
                                       fit_type = fit_type),
                       SIMPLIFY = FALSE)
        out3 <- unlist(out3,
                       recursive = FALSE)
      } else {
        out3 <- all_indirect_paths_i(adj = beta[[1]],
                                     exclude = exclude,
                                     x = x,
                                     y = y,
                                     fit = fit,
                                     fit_type = fit_type)
      }

    class(out3) <- c("all_paths", class(out3))
    attr(out3, "call") <- match.call()
    out3
  }

#' @noRd

all_indirect_paths_i <- function(adj,
                                 exclude = NULL,
                                 x = NULL,
                                 y = NULL,
                                 fit = NULL,
                                 fit_type = NULL) {
    adj[adj > 0] <- 1
    adj <- t(adj)
    # Remove excluded variables
    if (is.character(exclude)) {
        adj <- adj[!(rownames(adj) %in% exclude),
                   !(colnames(adj) %in% exclude)]
      }

    # Remove variables that are only an indicator
    if (identical(fit_type, "lavaan")) {
        eqs_xy <- union(lavaan::lavNames(fit, type = "eqs.y"),
                        lavaan::lavNames(fit, type = "eqs.x"))
        adj <- adj[rownames(adj) %in% eqs_xy,
                   colnames(adj) %in% eqs_xy]
      }

    # Pure x variables
    tmp <- apply(adj, MARGIN = 2,
                 FUN = function(x) {identical(range(x), c(0, 0))})
    x_only <- colnames(adj)[tmp]
    x_all <- union(rownames(adj), colnames(adj))
    y_all <- x_all[!(x_all %in% x_only)]

    # Keep only user-specified variables
    if (!is.null(x)) {
        x_all <- x_all[x_all %in% x]
        if (isTRUE(length(x_all) == 0)) {
            stop("None of the eligible x variables are on the requested list.")
          }
      }
    if (!is.null(y)) {
        y_all <- y_all[y_all %in% y]
        if (isTRUE(length(y_all) == 0)) {
            stop("None of the eligible y variables are on the requested list.")
          }
      }

    # Enumerate pairs
    xy_pairs <- expand.grid(y = y_all,
                            x = x_all,
                            stringsAsFactors = FALSE)
    graph_adj <- igraph::graph_from_adjacency_matrix(adj,
                            mode = "directed")
    out <- mapply(igraph::all_simple_paths,
                  from = xy_pairs$x,
                  to = xy_pairs$y,
                  MoreArgs = list(graph = graph_adj),
                  SIMPLIFY = FALSE)
    out <- out[sapply(out, length) > 0]
    out <- unlist(out, recursive = FALSE)

    # Keep only paths with one or more mediators
    out1 <- out[sapply(out, length) > 2]
    out2 <- unname(lapply(out1, names))

    # Format the output
    out3 <- lapply(out2, to_x_y_m)
    names(out3) <- sapply(out3, path_name)
    out3
  }

#' @describeIn all_indirect_paths Convert the output of
#' [all_indirect_paths()] to a data frame with
#' three columns: `x`, `y`, and `m`.
#'
#' @order 2
#'
#' @export

all_paths_to_df <- function(all_paths) {
    all_x <- sapply(all_paths, function(x) x$x)
    all_y <- sapply(all_paths, function(x) x$y)
    all_m <- sapply(all_paths, function(x) x$m,
                    simplify = FALSE)
    out <- data.frame(x = all_x,
                      y = all_y)
    out$m <- all_m
    out
  }

#' @noRd
# Vector to `x`, `y`, and `m`

to_x_y_m <- function(x) {
    if (isTRUE(length(x) == 2)) {
        out <- list(x = x[1],
                    y = x[2],
                    m = NULL)
        return(out)
      }
    if (length(x) > 2) {
        p <- length(x)
        out <- list(x = x[1],
                    y = x[p],
                    m = x[-c(1, p)])
        return(out)
      }
    NA
  }

#' @noRd
# Create path name from a vector of x, y, and m

path_name <- function(obj) {
    vars <- c(obj$x, obj$m, obj$y)
    out <- paste(vars, collapse = " -> ")
    out
  }

#' @noRd
# Create beta from a list of lm outputs

beta_from_lm <- function(fit) {
    ptable <- lm2ptable(fit)$est
    tmp <- ptable[ptable$op == "~", ]
    vars <- union(tmp$lhs, tmp$rhs)
    p <- length(vars)
    out <- matrix(0, nrow = p, ncol = p)
    colnames(out) <- vars
    rownames(out) <- vars
    for (i in seq_len(nrow(tmp))) {
        out[tmp[i, "lhs"], tmp[i, "rhs"]] <- 1
      }
    out
  }