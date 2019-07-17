## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(DataSpaceR)
con <- connectDS()

DT::datatable(con$mabGrid, options = list(autoWidth = TRUE, scrollX = TRUE))

## ------------------------------------------------------------------------
# filter the grid by viruses
con$filterMabGrid(using = "viruses", value = c("242-14", "Q23.17", "6535.3", "BaL.26", "DJ263.8"))

# filter the grid by donor species (llama)
con$filterMabGrid(using = "donor_species", value = "llama")

# check the updated grid
DT::datatable(con$mabGrid, options = list(autoWidth = TRUE, scrollX = TRUE))

## ----eval=FALSE----------------------------------------------------------
#  con$
#    filterMabGrid(using = "hxb2_location", value = c("Env", "Gag"))$
#    filterMabGrid(using = "donor_species", value = "llama")$
#    mabGrid

## ------------------------------------------------------------------------
# retrieve available viruses in the filtered grid
con$retrieveMabGridValue(using = "viruses")

# retrive available clades for 1H9 mAb mixture in the filtered grid
con$retrieveMabGridValue(using = "clades", mab_mixture = "1H9")

## ------------------------------------------------------------------------
mab <- con$getMab()
mab

## ------------------------------------------------------------------------
DT::datatable(mab$nabMab, options = list(autoWidth = TRUE, scrollX = TRUE))

## ----session-info--------------------------------------------------------
sessionInfo()

