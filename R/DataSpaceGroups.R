#' @title  The DataSpaceGroups class
#'
#' @description
#' An R6 class for DataSpace Groups data.
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getGroups()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Get group by `group_id` or pass a filtered `availableGroups` object.
#' groups <- con$getGroups(c(266, 267))
#' groups <- con$availableGroups[label == "NYVAC durability comparison"] |>
#'   con$getGroups()
#'
#' # Retrieving group assay data for cvd408 from
#' # DataSpace is done automatically when the groups object is created.
#' groups$datasets$BAMA
#'
#' # Get variable information of the assay dataset
#' groups$datasetDescription$BAMA
#'
#' }
#'
DataSpaceGroups <- R6Class(
  classname = "DataSpaceGroups",
  inherit = DataSpaceConnection,
  public = list(

    #' @description
    #' Initialize `DataSpaceGroups` class.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param groupIds A character vecotor of `group_id` values.
    #' as URL, path and username.
    initialize = function(groupIds = NULL) {

      private$.groupIds <- groupIds
      private$.groupLabels <- private$.shared$.availableGroups[group_id %in% groupIds, label]
      private$.groupNames <- private$.shared$.availableGroups[group_id %in% groupIds, original_label]

      private$.groups <- private$.groupLabels
      names(private$.groups) <-  private$.groupNames

      private$.studyIds <- private$.shared$.availableGroups[group_id %in% groupIds, studies] |>
        strsplit(", ") |>
        unlist() |>
        unique()

      private$.mabMixIds <- private$.shared$.mabStudies[
        strsplit(studies_available, ", ") |> sapply(\(sa) any(sa %in% private$.studyIds)),
        mab_mix_id
      ] |>
        unique()

      private$.mabIds <- private$.shared$.mabMix[mab_mix_id %in% private$.mabMixIds, mab_id] |>
        unique()

      self$refresh()

      NULL
    },

    #' @description
    #' Print \code{DataSpaceStudy} class.
    print = function() {

      cat("<DataSpaceGroups>")
      cat("\n  Groups:", paste(private$.groups, collapse = ", "))
      cat("\n  Available integrated datasets:")
      if (nrow(private$.availableIntegratedDatasets) > 0) {
        cat(paste("\n    -", unique(private$.availableIntegratedDatasets$assay_label)), sep = "")
      }
      cat("\n  Available Groups objects:")
      cat(paste0("\n    - ", DataSpaceMabs$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection objects:")
      cat(paste0("\n    - ", DataSpaceConnection$active |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n  Available Connection methods:")
      cat(paste0("\n    - ", DataSpaceConnection$public_methods |> names() |> cleanReservedDataSpaceR6()), sep = "")
      cat("\n")
    },

    #' @description
    #' Refresh loaded integrated datasets, and information of what datasets are available.
    refresh = function() {

      private$.getIntegratedAvailableDatasets()

      datasets <- Map(
        \(assay, group, groupLabel) {
          labkey.selectRows(
            baseUrl=private$.shared$.config$labkeyUrlBase,
            folderPath="/CAVD",
            schemaName="study",
            queryName=assay,
            viewName="",
            colFilter=makeFilter(c(sprintf("SubjectId/%s", group), "EQUAL", groupLabel)),
            containerFilter=NULL,
            colNameOpt="rname"
          ) |>
            setDT() |>
            _[, group := group]
        },
        private$.availableIntegratedDatasets$assay_identifier,
        private$.availableIntegratedDatasets$group,
        private$.availableIntegratedDatasets$group_label
      ) |>
        (\(.) {
          assays <- sort(unique(names(.)))
          ds <- lapply(
            assays,
            \(a)
            .[names(.) == a] |>
              rbindlist()
          )
          names(ds) <- assays
          return(ds)
        })()

      setDatasetNames(datasets)
      private$.datasets <- datasets

      varInfo <- lapply(
        sort(unique(private$.availableIntegratedDatasets$assay_identifier)),
        \(assay)
        getVarInfo(
          assay,
          names(private$.datasets[[assay]]),
          private$.shared$.config$labkeyUrlBase
        )
      )

      private$.variableDefinitions <- varInfo
      names(private$.variableDefinitions) <- sort(unique(private$.availableIntegratedDatasets$assay_identifier))

    }
  ),
  active = list(
    #' @field availableDatasets A data.table of datasets available in
    #' the object.
    availableDatasets = function() {
      private$.availableIntegratedDatasets
    },

    #' @field datasets A list of data.table objects containing the
    #' availableDatasets that were loaded.
    datasets = function() {
      private$.datasets
    },

    #' @field variableDefinitions A data.table of variable definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }

  ),
  private = list(
    .groupIds = character(),
    .groupLabels = character(),
    .groupNames = character(),
    .groups = character(),
    .studyIds = character(),
    .mabMixIds = character(),
    .datasets = list(),
    .availableIntegratedDatasets = data.table(),
    .variableDefinitions = list(),

    .getIntegratedAvailableDatasets = function() {

      private$.availableIntegratedDatasets <-
        Map(\(gn, gl) {
          labkey.executeSql(
            baseUrl = private$.shared$.config$labkeyUrlBase,
            folderPath = "CAVD",
            schemaName = "study",
            sql = datasetCountQuery(gn, gl),
            colNameOpt = "fieldname"
          ) |>
            setDT() |>
            _[,.(group = gn, group_label = gl, assay_identifier, assay_label, n)]
        }, private$.groupNames, private$.groupLabels) |>
        rbindlist()

    }
  )
)
