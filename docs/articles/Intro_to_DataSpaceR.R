## ----connectDS-----------------------------------------------------------
library(DataSpaceR)
con <- connectDS()
con

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
knitr::kable(cvd256$getVariableInfo("NAb"))

## ----getDataset-filter---------------------------------------------------
library(Rlabkey)
cvd256Filter <- makeFilter(c("visit_day", "EQUAL", "0"))
NAb_day0 <- cvd256$getDataset("NAb", colFilter = cvd256Filter)
dim(NAb_day0)

## ----cross-connection----------------------------------------------------
cavd <- con$getStudy("")

## ----cross-connection-print----------------------------------------------
cavd
knitr::kable(cavd$availableDatasets)

## ----cross-connection-dem------------------------------------------------
conFilter <- makeFilter(c("species", "EQUAL", "Human"))
human <- cavd$getDataset("Demographics", colFilter = conFilter)
dim(human)
colnames(human)

## ----availableGroups-----------------------------------------------------
con$availableGroups

## ----group-connection----------------------------------------------------
mice <- con$getStudy("", groupId = 216)
mice

## ----group-connection-nab------------------------------------------------
NAb_mice <- mice$getDataset("NAb")
dim(NAb_mice)

## ------------------------------------------------------------------------
library(pryr)
cvd408 <- con$getStudy("cvd408")

str(cvd408$cache)
object_size(cvd408)

## ------------------------------------------------------------------------
ptm <- proc.time()
invisible(cvd408$getDataset("NAb"))
proc.time() - ptm

str(cvd408$cache, max.level = 2)
object_size(cvd408)

## ------------------------------------------------------------------------
ptm <- proc.time()
invisible(cvd408$getDataset("NAb"))
proc.time() - ptm

str(cvd408$cache, max.level = 2)
object_size(cvd408)

## ------------------------------------------------------------------------
cvd408$clearCache()

str(cvd408$cache, max.level = 2)
object_size(cvd408)

## ----session-info--------------------------------------------------------
sessionInfo()

