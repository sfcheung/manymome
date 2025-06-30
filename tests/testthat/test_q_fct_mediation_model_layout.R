skip("WIP")

skip_on_cran()

library(testthat)
library(manymome)
library(lavaan)

test_that("q function: model", {

tmp1 <- paths_to_models(c("x1 -> m11 -> m12 -> y1",
                          "x1 -> m2 -> y1"))
lm_form1 <- form_models_paths(tmp1)

tmp2 <- paths_to_models(
          list(c("x1", "m11"),
              c("m11", "m12", "y1"),
              c("m11", "m2", "y1"),
              c("x1", "y1")))

lm_form2 <- form_models_paths(tmp2)

fit1 <- lavaan::sem(lm_form1,
                    do.fit = FALSE)
betam1 <- lavaan::lavInspect(fit1, "free")$beta
xi <- which(colnames(betam1) == "x1")
yi <- which(colnames(betam1) == "y1")
i <- c(xi, setdiff(seq_len(ncol(betam1)), c(xi, yi)), yi)
betam1 <- betam1[i, i]
betam1

fit2 <- lavaan::sem(lm_form2,
                    do.fit = FALSE)
betam2 <- lavaan::lavInspect(fit2, "free")$beta
xi <- which(colnames(betam2) == "x1")
yi <- which(colnames(betam2) == "y1")
i <- c(xi, setdiff(seq_len(ncol(betam2)), c(xi, yi)), yi)
betam2 <- betam2[i, i]
mnames2 <- colnames(betam2)[c(-1, -5)]
tmp <- replicate(length(mnames2), mnames2, simplify = FALSE)
tmp <- do.call(expand.grid,
               tmp)
tmp <- tmp[apply(tmp, 1, \(x) length(unique(x))) == 3, ]
tmp
tmp <- apply(tmp, 1, \(x) unname(c("x1", x, "y1")), simplify = FALSE)
for (xx in tmp) {
  beta_tmp <- betam2[xx, xx]
  if (all(beta_tmp[upper.tri(beta_tmp)] == 0)) break
}
beta_tmp


})
