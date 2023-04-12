# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("do_boot.Rmd.originaL", output = "do_boot.Rmd")
knitr::knit("do_mc.Rmd.originaL", output = "do_mc.Rmd")
knitr::knit("manymome.Rmd.originaL", output = "manymome.Rmd")
knitr::knit("med_lav.Rmd.originaL", output = "med_lav.Rmd")
knitr::knit("med_lm.Rmd.originaL", output = "med_lm.Rmd")
knitr::knit("mome_lm.Rmd.original", output = "mome_lm.Rmd")

setwd(base_dir)
