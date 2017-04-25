.DSCon <- setRefClass(
  Class = "DataSpaceConnection",
  fields = list(
    study = "character",
    config = "list",
    available_datasets = "data.frame"
  ),
  methods = list(
    show = function() {
      url <- file.path(gsub("/$", "", config$labkey.url.base), "project", gsub("^/", "", config$labkey.url.path), "begin.view")
      cat(paste0("DataSpace Connection to ", study))
      cat(paste0("\nURL: ", url))
      cat(paste0("\nUser: ", config$labkey.user.email))
      cat("\nAvailable datasets")
      for(i in 1:nrow(available_datasets)) {
        cat(paste0("\n\t", available_datasets[i, "Name"]))
      }
    }
  )
)
