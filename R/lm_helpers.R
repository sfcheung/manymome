
coef2lor <- function(x) {
    y <- all.vars(stats::formula(x))[1]
    bs <- stats::coef(x)
    bnames <- names(bs)
    j <- which(bnames == "(Intercept)")
    k <- length(bs) - 1
    out <- data.frame(lhs = rep(y, k),
                      op = "~",
                      rhs = bnames[-j],
                      est = bs[-j])
    out <- rbind(out,
                 data.frame(lhs = y,
                            op = "~1",
                            rhs = "",
                            est = bs[j]))
    out
  }

get_response <- function(x) {
    all.vars(stats::formula(x))[1]
  }

get_response_data <- function(x) {
    y <- all.vars(stats::formula(x))[1]
    stats::model.frame(x)[, y, drop = FALSE]
  }

lm2mod_i <- function(x) {
    y <- all.vars(stats::formula(x))[1]
    out <- paste(y, "~",
            paste(names(stats::coef(x))[-1], collapse = " + "))
    out
  }

lm2mod <- function(outputs, dat) {
    out <- lapply(outputs, lm2mod_i)
    mod <- paste(out, collapse = "\n")
    mod
  }

merge_model_matrix <- function(outputs) {
    mm <- lapply(outputs,
                 function(x) {
                    out <- stats::model.matrix(x,
                              contrasts.arg = x$contrasts)[, -1]
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
        y1 <- y[, ykeep]
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

factor2var <- function(x_value, x_contrasts) {
    x_fac <- factor(x_value)
    stats::contrasts(x_fac) <- x_contrasts
    m <- do.call(x_contrasts, list(n = levels(x_fac)))
    out <- t(sapply(x_value, function(x) m[x, ]))
    out
  }
