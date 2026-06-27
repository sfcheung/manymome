#' @noRd
add_cov_with_w <- function(
  sem_model,
  ...
) {
  fit0 <- lavaan::sem(
    sem_model,
    ...,
    do.fit = FALSE
  )
  pt0 <- lavaan::parameterTable(fit0)
  model_cov0 <- w_cov_with_y(pt0)
  if (length(model_cov0) == 0) {
    return(NULL)
  }
  requireNamespace("lavaan", quietly = TRUE)
  model_cov1 <- semhelpinghands::auto_exo_cov(
                  paste0(c(sem_model, model_cov0), collapse = "\n"),
                  FUN = "sem",
                  print = FALSE
                )
  out <- paste0(
    c(model_cov0,
      model_cov1),
    collapse = "\n"
  )
  out
}

#' @noRd
w_cov_with_y <- function(
  ptable
) {
  # A model always has m/y variables
  ys <- lavaan::lavNames(ptable, "eqs.y")
  w_terms <- prods_from_pt(ptable)
  i <- sapply(
          w_terms,
          function(x) {
            any(ys %in% x)
          }
        )
  w_terms <- w_terms[i]
  if (length(w_terms) == 0) {
    return(character(0))
  }
  out <- lapply(
    w_terms,
    function(a) {
      c(paste(a[1], "~~", paste0(a[1:2], collapse = ":")),
        paste(a[2], "~~", paste0(a[1:2], collapse = ":")))
    })
  unname(unlist(out))
}

#' @noRd
prods_from_pt <- function(
  ptable
) {
  a <- ptable$rhs
  a <- strsplit(a, ":", fixed = TRUE)
  i <- sapply(
          a,
          length
        )
  names(a) <- ptable$rhs
  a <- a[i > 1]
  a[!duplicated(a)]
}

#' @noRd
std_prods <- function(
  ptable,
  fit
) {
  xw <- prods_from_pt(ptable)
  if (length(xw) == 0) {
    return(ptable)
  }
  ptable$std.prod <- NA_real_
  sd_all <- lavaan::lavInspect(
    fit,
    "implied"
  )$cov
  sd_all <- sqrt(diag(sd_all))
  for (i in seq_along(xw)) {
    xw_name <- names(xw)[i]
    xw_i <- xw[[i]]
    tmp <- (ptable$rhs == xw_name) &
           (ptable$op == "~")
    if (!any(tmp)) next
    b <- which(tmp)
    for (j in b) {
      y_j <- ptable[j, "lhs"]
      x_i <- xw_i[1]
      w_i <- xw_i[2]
      ptable[j, "std.prod"] <- ptable[j, "est"] *
        (sd_all[x_i] * sd_all[w_i]) / sd_all[y_j]
    }
  }
  ptable
}
