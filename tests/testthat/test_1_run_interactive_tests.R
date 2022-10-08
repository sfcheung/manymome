skip("For running all interactive session tests")
# Work-in-progress

library(testthat)

fn_all <- list.files("./tests/testthat/",
              full.names = TRUE,
              include.dirs = FALSE)
fn_all <- fn_all[!grepl("./tests/testthat/_snaps", fn_all, fixed = TRUE)]
fn_all <- fn_all[!grepl("./tests/testthat/test-1_run_interactive_session_tests.R", fn_all, fixed = TRUE)]
# txt_str <- 'skip("Test parallel processing: Test in interactive sections")'
txt_str <- 'skip("Test parallel processing: Test in interactive sections")'
is_inter <- sapply(fn_all, function(x) {
    suppressWarnings(tmp <- readLines(x))
    isTRUE(any(grepl(txt_str, tmp, fixed = TRUE)))
  })

fn_inter <- fn_all[is_inter]

# Sys.setenv("TEST_SLOW" = "true")
# Sys.setenv("NOT_CRAN" = "true")

for (i in fn_inter) {
    test_file(i)
  }

Sys.unsetenv("TEST_SLOW" = "")
Sys.unsetenv("NOT_CRAN")