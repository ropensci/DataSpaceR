## ---- eval=FALSE---------------------------------------------------------
#  DataSpaceR::writeNetrc("yourEmail@address.com", "yourSecretPassword")

## ----connectDS-----------------------------------------------------------
library(DataSpaceR)
con <- connectDS()
con

## ----availableStudies----------------------------------------------------
knitr::kable(head(con$availableStudies))

## ----getStudy------------------------------------------------------------
cvd256 <- con$getStudy("cvd256")
cvd256

## ----other-fields--------------------------------------------------------
knitr::kable(cvd256$availableDatasets)
knitr::kable(cvd256$treatmentArm)

## ----getDataset----------------------------------------------------------
NAb <- cvd256$getDataset("NAb")
dim(NAb)
colnames(NAb)

## ----getVariableInfo-----------------------------------------------------
knitr::kable(cvd256$getDatasetDescription("NAb"))

## ----getDataset-filter---------------------------------------------------
cvd256Filter <- makeFilter(c("visit_day", "EQUAL", "0"))
NAb_day0 <- cvd256$getDataset("NAb", colFilter = cvd256Filter)
dim(NAb_day0)

## ----connection-all-studies----------------------------------------------
cavd <- con$getStudy("")

## ----connection-all-studies-datasets-------------------------------------
cavd
knitr::kable(cavd$availableDatasets)

## ----connection-all-studies-dem------------------------------------------
conFilter <- makeFilter(c("species", "EQUAL", "Human"))
human <- cavd$getDataset("Demographics", colFilter = conFilter)
dim(human)
colnames(human)

## ----availableGroups-----------------------------------------------------
knitr::kable(con$availableGroups)

## ----group-connection----------------------------------------------------
nyvac <- con$getGroup(220)
nyvac

## ----group-connection-nab------------------------------------------------
NAb_nyvac <- nyvac$getDataset("NAb")
dim(NAb_nyvac)

## ----session-info--------------------------------------------------------
sessionInfo()

