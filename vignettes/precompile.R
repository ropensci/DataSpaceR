# Precompiled vignettes that depend on API key
# Must manually move image files from / to /vignettes after knit
# https://ropensci.org/technotes/2019/12/08/precompute-vignettes/
# https://github.com/ropensci/eia/blob/master/vignettes/precompile.R

library(knitr)
Sys.setenv("NOT_CRAN" = "true")
knit("vignettes/DataSpaceR.Rmd.og", "vignettes/DataSpaceR.Rmd")
knit("vignettes/Monoconal_Antibody_Data.Rmd.og", "vignettes/Monoconal_Antibody_Data.Rmd")
knit("vignettes/Non_Integrated_Datasets.Rmd.og", "vignettes/Non_Integrated_Datasets.Rmd")
knit("vignettes/Publication_Data.Rmd.og", "vignettes/Publication_Data.Rmd")
