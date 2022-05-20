#' @title Print the a 'indirect' Class Object
#'
#' @description Print the content of the output of [indirect()]
#'              or [cond_indirect()].
#'
#' @return
#'  `x` is returned invisibly.
#'
#' @param x The output of he output of [indirect()]
#'              or [cond_indirect()].
#' @param digits Number of digits to display. Default is 3.
#' @param ...  Other arguments. Not used.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' library(lavaan)
#' dat <- modmed_x1m3w4y1
#' mod <-
#' "
#' m1 ~ a1 * x   + b1 * w1 + d1 * x:w1
#' m2 ~ a2 * m1  + b2 * w2 + d2 * m1:w2
#' m3 ~ a3 * m2  + b3 * w3 + d3 * m2:w3
#' y  ~ a4 * m3  + b4 * w4 + d4 * m3:w4
#' "
#' fit <- sem(mod, dat, meanstructure = TRUE, fixed.x = FALSE, se = "none", baseline = FALSE)
#' est <- parameterEstimates(fit)
#'
#' wvalues <- c(w1 = 5, w2 = 4, w3 = 2, w4 = 3)
#'
#' indirect_1 <- indirect(x = "x", y = "y", m = c("m1", "m2", "m3"), fit = fit,
#'                        wvalues = wvalues)
#' indirect_2 <- (est[est$label == "a1", "est"] +
#'                 wvalues["w1"] * est[est$label == "d1", "est"]) *
#'               (est[est$label == "a2", "est"] +
#'                 wvalues["w2"] * est[est$label == "d2", "est"]) *
#'               (est[est$label == "a3", "est"] +
#'                 wvalues["w3"] * est[est$label == "d3", "est"]) *
#'               (est[est$label == "a4", "est"] +
#'                 wvalues["w4"] * est[est$label == "d4", "est"])
#' indirect_1$indirect
#' indirect_2
#'
#' @export

print.indirect <- function(x, digits = 3, ...) {
    xold <- x
    my_call <- x$call
    wvalues <- x$wvalues
    standardized_x <- x$standardized_x
    standardized_y <- x$standardized_y
    standardized <- (standardized_x && standardized_y)
    has_boot_ci <- isTRUE(!is.null(x$boot_ci))
    has_w <- isTRUE(!is.null(wvalues))
    if (has_w) {
        w0 <- wvalues
        wnames <- names(w0)
      } else {
        w0 <- NA
        wnames <- NA
      }
    has_m <- isTRUE(!is.null(x$m))
    if (has_m) {
        m0 <- x$components
        m0c <- x$components_conditional
        mnames <- x$m
        mpathnames <- names(m0)
      } else {
        m0 <- NA
        m0c <- NA
        mnames <- NA
        mpathnames <- NA
      }
    x0 <- x$x
    y0 <- x$y
    if (has_m) {
        path <- paste(x0, "->",
                      paste(mnames, collapse = " -> "),
                      "->", y0)
      } else {
        path <- paste(x0, "->", y0)
      }
    std_str <- ""
    if (standardized) {
        std_str <- paste0("(Both ", sQuote(x0),
                          " and ", sQuote(y0), " Standardized)")
      } else {
        if (standardized_x) {
            std_str <- paste0("(",sQuote(x0), " Standardized)")
          }
        if (standardized_y) {
            std_str <- paste0("(", sQuote(y0), " Standardized)")
          }
      }
    cond_str <- ""
    cond_str2 <- ""
    if (has_m) {
        cond_str <- "indirect"
        cond_str2 <- "Indirect"
      }
    if (has_w) {
        cat("\n== Conditional", cond_str2, "Effect",
            std_str, " ==")
      } else {
        cat("\n==", cond_str2, "Effect ==")
      }
    cat("\n")
    # cat("\nPath: ", path)
    ptable <- data.frame(Factor = "Path:", Value = path)
    if (has_w) {
      # cat("\nModerators: ", paste0(wnames, collapse = ", "))
      ptable <- rbind(ptable,
                      c("Moderators:", paste0(wnames, collapse = ", ")))
    }
    if (has_boot_ci) {
        b_str1 <- paste0(formatC(x$level * 100, 1, format = "f"), "%",
                         " Bootstrap Confidence Interval:")
        b_str2 <- paste0("[",
                         paste0(formatC(x$boot_ci, digits, format = "f"),
                                collapse = " to "),
                         "]")
        b_str1b <- "Null Hypothesis Significant Test:"
        b_str2b <- ifelse((x$boot_ci[1] > 0) || (x$boo_ci[2] < 0),
                          paste0("Sig. (Level of Significance ",
                                formatC(1 - x$level, digits, format = "f"), ")"),
                          paste0("Not Sig. (Level of Significance ",
                                formatC(1 - x$level, digits, format = "f"), ")"))
        b_row <- c(b_str1, b_str2)
      }
    if (has_w) {
        # cat("\nConditional", cond_str, "effect",
        #     "from", sQuote(x0), "to", sQuote(y0),
        #     ":", formatC(x$indirect, digits = digits, format = "f"))
        ptable <- rbind(ptable,
                        c(ifelse(has_m, "Conditional Indirect Effect:",
                                        "Conditional Effect:"),
                          formatC(x$indirect, digits = digits, format = "f")))
        tmp <- paste(paste(wnames, "=", w0), collapse = ", ")
        # cat("\nWhen:", tmp)
        if (has_boot_ci) {ptable <- rbind(ptable, b_row)}
        ptable <- rbind(ptable,
                        c("When:", tmp))
      } else {
        # if (has_m) {cat("\nEffect ")} else {cat("\nIndirect Effect ")}
        # cat("from", sQuote(x0), "to", sQuote(y0),
        #     ":", formatC(x$indirect, digits = digits, format = "f"))
        ptable <- rbind(ptable,
                        c(ifelse(has_m, "Indirect Effect", "Effect"),
                          formatC(x$indirect, digits = digits, format = "f")))
        if (has_boot_ci) {ptable <- rbind(ptable, b_row)}
      }
    ptable <- data.frame(lapply(ptable, format))
    colnames(ptable) <- c("", "")
    print(ptable, row.names = FALSE)
    if (has_boot_ci) {
        cat("\nPercentile confidence interval formed by nonparametric bootstrapping",
            "with", length(x$boot_indirect), "bootstrap samples.")
      }
    if (has_m) {
        if (has_w) {
          out <- data.frame(mpathnames, m0c, m0)
          rownames(out) <- NULL
          colnames(out) <- c("Path", "Conditional Effect", "Original Coefficient")
        } else {
          out <- data.frame(mpathnames, m0)
          rownames(out) <- NULL
          colnames(out) <- c("Path", "Coefficient")
        }
        cat("\n")
        cat("\nCoefficients of Component Paths:")
        cat("\n")
        print(out, digits = digits, row.names = FALSE)
        if (standardized_x || standardized_y) {
          cat("\nNOTE: The effects of the component paths are form the model, not standardized.")
        }
      }
    cat("\n")
    invisible(x)
  }
