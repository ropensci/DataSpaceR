## ----CreateConnection----------------------------------------------------
library(DataSpaceR)
cvd256 <- connectDS(study = "cvd256")
cvd256
cvd256$available_datasets

## ----getDataset----------------------------------------------------------
NAb <- cvd256$getDataset("NAb")
dim(NAb)
colnames(NAb)

## ----getDataset-filter---------------------------------------------------
library(Rlabkey)
cvd256Filter <- makeFilter(c("visit_day", "EQUAL", "0"))
NAb_day0 <- cvd256$getDataset("NAb", colFilter = cvd256Filter)
dim(NAb_day0)

## ----cross-connection----------------------------------------------------
con <- connectDS("")

## ----cross-connection-print----------------------------------------------
con
con$availableDatasets

## ----cross-connection-dem------------------------------------------------
conFilter <- makeFilter(c("species", "EQUAL", "Human"))
human <- con$getDataset("Demographics", colFilter = conFilter)
dim(human)
colnames(human)

## ------------------------------------------------------------------------
library(pryr)
cvd408 <- connectDS("cvd408")

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

