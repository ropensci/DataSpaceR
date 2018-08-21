## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(DataSpaceR)
con <- connectDS()

DT::datatable(con$mabGrid, options=list(autoWidth = TRUE, scrollX = TRUE))

## ------------------------------------------------------------------------
# filter the grid by HXB2 location (Env and 249M Gag)
con$filterMabGrid(using = "hxb2_location", value = c("Env", "Gag"))

# filter the grid by species (llama)
con$filterMabGrid(using = "donor_species", value = "llama")

# check the updated grid
DT::datatable(con$mabGrid, options=list(autoWidth = TRUE, scrollX = TRUE))

## ----eval=FALSE----------------------------------------------------------
#  # Or
#  con$filterMabGrid(using = "hxb2_location", value = c("Env", "Gag"))$filterMabGrid(using = "donor_species", value = "llama")$mabGrid

## ------------------------------------------------------------------------
# retrieve available viruses in the grid
con$retrieveMabGridValue(using = "viruses")

# retrive available clades for 1H9 mAb mixture in the grid
con$retrieveMabGridValue(using = "clades", mAb_mixture = "1H9")

## ------------------------------------------------------------------------
mab <- con$getMab()
mab

## ------------------------------------------------------------------------
DT::datatable(mab$nabMab, options=list(autoWidth = TRUE, scrollX = TRUE))

## ----session-info--------------------------------------------------------
sessionInfo()

