# Precompiled vignettes that depend on API key
# Must manually move image files from / to /vignettes after knit
# https://ropensci.org/technotes/2019/12/08/precompute-vignettes/
# https://github.com/ropensci/eia/blob/master/vignettes/precompile.R

library(knitr)
knit("vignettes/DataSpaceR.Rmd.orig", "vignettes/DataSpaceR.Rmd")
knit("vignettes/Monoconal_Antibody_Data.Rmd.orig", "vignettes/Monoconal_Antibody_Data.Rmd")
knit("vignettes/Non_Integrated_Datasets.Rmd.orig", "vignettes/Non_Integrated_Datasets.Rmd")
knit("vignettes/Publication_Data.Rmd.orig", "vignettes/Publication_Data.Rmd")
knit("vignettes/Virus_Name_Mapping_Tables.Rmd.orig", "vignettes/Virus_Name_Mapping_Tables.Rmd")
