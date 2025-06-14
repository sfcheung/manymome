
coef2lor <- function(x,
                     coefs_template = NULL) {
    y <- get_response(x)
    bs <- stats::coef(x)
    bnames <- names(bs)
    j <- which(bnames == "(Intercept)")
    k <- length(bs) - 1
    if (is.null(coefs_template)) {
      out <- data.frame(lhs = rep(y, k),
                        op = "~",
                        rhs = bnames[-j],
                        est = bs[-j])
      out <- rbind(out,
                  data.frame(lhs = y,
                              op = "~1",
                              rhs = "",
                              est = bs[j]))
    } else {
      i <- match(bnames[-j],
                 coefs_template$rhs)
      coefs_template[i, "est"] <- bs[-j]
      i <- which(coefs_template$op == "~1")
      coefs_template[i, "est"] <- bs[j]
      out <- coefs_template
    }
    out
  }

get_response <- function(x) {
    all.vars(stats::formula(x))[1]
  }

get_response_data <- function(x) {
    y <- get_response(x)
    stats::model.frame(x)[, y, drop = FALSE]
  }

lm2mod_i <- function(x) {
    y <- get_response(x)
    out <- paste(y, "~",
            paste(names(stats::coef(x))[-1], collapse = " + "))
    out
  }

lm2mod <- function(outputs) {
    out <- lapply(outputs, lm2mod_i)
    mod <- paste0(out, collapse = "\n")
    mod
  }


merge_model_frame <- function(outputs) {
    mm <- lapply(outputs,
                 function(x) {
                    out <- stats::model.frame(x)
                  })
    vnames <- unique(unlist(sapply(mm, colnames)))
    idname <- newname(vnames)
    mm1 <- lapply(mm, function(x) {
                          j <- colnames(x)
                          out <- cbind(x, seq_len(nrow(x)))
                          colnames(out) <- c(j, idname)
                          out
                        })
    # TODO: Check mm
    `%merge%` <- function(x, y) {
        xnames <- colnames(x)
        ynames <- colnames(y)
        ykeep <- c(idname, ynames[!(ynames %in% xnames)])
        y1 <- y[, ykeep, drop = FALSE]
        merge(x, y1,
              by = idname)
      }
    mm2 <- Reduce(`%merge%`, mm1)
    mm2[, idname] <- NULL
    mm2
  }

merge_model_matrix <- function(outputs) {
    mm <- lapply(outputs,
                 function(x) {
                    stats::model.matrix(x,
                      contrasts.arg = x$contrasts)[, -1, drop = FALSE]
                    # y_data <- get_response_data(x)
                    # cbind(y_data, out)
                  })
    mmy <- lapply(outputs,
                 function(x) {
                    as.matrix(get_response_data(x))
                  })
    mm <- c(mm, mmy)
    vnames <- unique(c(unlist(lapply(mm, colnames))))
    p <- length(vnames)
    n <- nrow(mm[[1]])
    mm_out <- matrix(
                NA,
                nrow = n,
                ncol = p
              )
    colnames(mm_out) <- vnames
    # Assume the rows match across models
    empty_cols <- vnames
    for (i in seq_along(mm)) {
      mm_i <- mm[[i]]
      if (length(empty_cols) == 0) {
        break
      }
      to_add <- intersect(colnames(mm_i),
                          empty_cols)
      mm_out[, to_add] <- mm_i[, to_add]
      empty_cols <- setdiff(empty_cols,
                            to_add)
    }
    data.frame(mm_out,
               check.names = FALSE)
  }

merge_model_matrix_old <- function(outputs) {
    mm <- lapply(outputs,
                 function(x) {
                    out <- stats::model.matrix(x,
                              contrasts.arg = x$contrasts)[, -1, drop = FALSE]
                    y_data <- get_response_data(x)
                    cbind(y_data, out)
                  })
    vnames <- unique(unlist(sapply(mm, colnames)))
    idname <- newname(vnames)
    mm1 <- lapply(mm, function(x) {
                          j <- colnames(x)
                          out <- cbind(x, seq_len(nrow(x)))
                          colnames(out) <- c(j, idname)
                          out
                        })
    # TODO: Check mm
    `%merge%` <- function(x, y) {
        xnames <- colnames(x)
        ynames <- colnames(y)
        ykeep <- c(idname, ynames[!(ynames %in% xnames)])
        y1 <- y[, ykeep, drop = FALSE]
        merge(x, y1,
              by = idname)
      }
    mm2 <- Reduce(`%merge%`, mm1)
    mm2[, idname] <- NULL
    mm2
  }

newname <- function(x) {
    out <- x[1]
    while (out %in% x) {
        out <- make.names(substring(tempfile(pattern = "id_", tmpdir = ""), 2))
      }
    out
  }

data2implied <- function(data) {
    cov <- cov(data)
    mean <- colMeans(data)
    list(cov = cov,
         mean = mean)
  }

get_mm_names <- function(x) {
    out1 <- colnames(stats::model.matrix(x))[-1]
    out2 <- get_response(x)
    c(out1, out2)
  }

check_except <- function(x) {
    # Check if a model is "disconnected" from other models.
    k <- length(x)
    out <- sapply(seq_len(k),
             function(i) {
                  isTRUE(any(x[[i]] %in%
                                unique(unlist(x[-i]))))
               })
    if (isTRUE(all(out))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
  }

check_cases <- function(x) {
    # Check whether the same cases are used
    k <- length(x)
    if (k == 1) return(TRUE)
    for (i in seq(2, k)) {
        for (j in seq_len(i - 1)) {
            namesi <- colnames(x[[i]])[-1]
            namesj <- colnames(x[[j]])[-1]
            names0 <- intersect(namesi, namesj)
            if (length(names0) == 0) next
            dati <- x[[i]][, names0]
            datj <- x[[j]][, names0]
            if (!identical(datj, dati)) {
                return(FALSE)
              }
          }
      }
    return(TRUE)
  }

auto_lm2list <- function(object) {
    if (inherits(object, "lm")) {
        out <- lm2list(object)
        return(out)
      }
    object
  }