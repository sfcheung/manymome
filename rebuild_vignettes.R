# Adapted from https://www.kloppenborg.ca/2021/06/long-running-vignettes/

base_dir <- getwd()

setwd("vignettes/")
knitr::knit("do_boot.Rmd.originaL", output = "do_boot.Rmd")
knitr::knit("manymome.Rmd.original", output = "manymome.Rmd")
knitr::knit("med_lav.Rmd.original", output = "med_lav.Rmd")
knitr::knit("med_lm.Rmd.original", output = "med_lm.Rmd")
knitr::knit("mome_lm.Rmd.original", output = "mome_lm.Rmd")
knitr::knit("do_mc.Rmd.original", output = "do_mc.Rmd")
knitr::knit("do_mc_lavaan_mi.Rmd.original", output = "do_mc_lavaan_mi.Rmd")

setwd(base_dir)

# For articles

base_dir <- getwd()

setwd("vignettes/articles")
knitr::knit("med_mg.Rmd.original", output = "med_mg.Rmd")
knitr::knit("q_mediation.Rmd.original", output = "q_mediation.Rmd")

setwd(base_dir)
