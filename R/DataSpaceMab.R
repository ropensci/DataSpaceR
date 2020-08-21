#' The DataSpaceMab class
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getMab()}
#'
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#'
#' @examples
#' \dontrun{
#' # Create a connection (Initiate a DataSpaceConnection object)
#' con <- connectDS()
#'
#' # Browse the mAb Grid
#' con$mabGridSummary
#'
#' # Filter the grid by viruses
#' con$filterMabGrid(using = "virus", value = c("242-14", "Q23.17", "6535.3", "BaL.26", "DJ263.8"))
#'
#' # Filter the grid by donor species (llama)
#' con$filterMabGrid(using = "donor_species", value = "llama")
#'
#' # Check the updated grid
#' con$mabGridSummary
#'
#' # Retrieve available viruses in the filtered grid
#' con$mabGrid[, unique(virus)]
#'
#' # Retrieve available clades for 1H9 mAb mixture in the filtered grid
#' con$mabGrid[mab_mixture == "1H9", unique(clade)]
#'
#' # Create a DataSpaceMab object that contains the filtered mAb data
#' mab <- con$getMab()
#' mab
#'
#' # Inspect the `nabMab` field
#' mab$nabMab
#' }
#'
#' @importFrom data.table setnames
DataSpaceMab <- R6Class(
  classname = "DataSpaceMab",
  public = list(

    #' @description
    #' Initialize \code{DataSpaceMab} object.
    #' See \code{\link{DataSpaceConnection}}.
    #' @param mabMixture A character vector.
    #' @param filters A list.
    #' @param config A list.
    initialize = function(mabMixture, filters, config) {
      assert_that(!is.null(config))

      # set primary fields
      private$.config <- config
      private$.filters <- filters
      private$.mabMixture <- mabMixture

      # get extra fields if available
      self$refresh()

      NULL
    },

    #' @description
    #' Print the \code{DataSpaceMab} object summary.
    print = function() {
      cat("<DataSpaceMab>")
      cat("\n  URL:", private$.config$labkeyUrlBase)
      cat("\n  User:", private$.config$labkeyUserEmail)
      cat("\n  Summary:")
      cat("\n    -", length(unique(private$.nabMab$prot)), "studies")
      cat("\n    -", length(unique(private$.nabMab$mab_mix_name_std)), "mAb mixtures")
      cat("\n    -", length(unique(private$.nabMab$neutralization_tier)), "neutralization tiers")
      cat("\n    -", length(unique(private$.nabMab$clade)), "clades")
      cat("\n  Filters:")
      if (length(private$.filters) > 0) {
        lapply(names(private$.filters), function(x) {
          cat("\n    - ", x, ": ", paste(private$.filters[[x]], collapse = ", "), sep = "")
        })
      } else {
        cat(" NA")
      }
      cat(" \n")
    },

    #' @description
    #' Refresh the \code{DataSpaceMab} object to update datasets.
    refresh = function() {
      tries <- c(
        class(try(
          private$.getNabMabs(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getMabs(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getStudies(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getAssays(),
          silent = !private$.config$verbose
        )),
        class(try(
          private$.getVariableDefinitions(),
          silent = !private$.config$verbose
        ))
      )

      invisible(!"try-error" %in% tries)
    }
  ),

  active = list(

    #' @field config A list. Stores configuration of the connection object such
    #' as URL, path and username.
    config = function() {
      private$.config
    },

    #' @field studyAndMabs A data.table. The table of available mAbs by study.
    studyAndMabs = function() {
      unique(private$.nabMab[, .(prot, mab_mix_id, mab_mix_label, mab_mix_name_std)])
    },

    #' @field mabs A data.table. The table of available mAbs and their
    #' attributes.
    mabs = function() {
      private$.mabs
    },

    #' @field nabMab A data.table. The table of mAbs and their neutralizing
    #' measurements against viruses.
    nabMab = function() {
      private$.nabMab
    },

    #' @field studies A data.table. The table of available studies.
    studies = function() {
      private$.studies
    },

    #' @field assays A data.table. The table of assay status by study.
    assays = function() {
      private$.assays
    },

    #' @field variableDefinitions A data.table. The table of variable
    #' definitions.
    variableDefinitions = function() {
      private$.variableDefinitions
    }
  ),

  private = list(
    .config = list(),
    .filters = list(),
    .mabMixture = character(),
    .mabs = data.table(),
    .nabMab = data.table(),
    .studies = data.table(),
    .assays = data.table(),
    .variableDefinitions = data.table(),

    .getNabMabs = function() {
      filters <- private$.filters
      if (length(filters) > 0) {
        filters <- lapply(names(filters), function(x) {
          makeFilter(c(x, "IN", paste(unique(unlist(lapply(filters[[x]], URLencode, reserved = TRUE))), collapse = ";")))
        })
        mabFilters <- unique(
          rbind(
            makeFilter(c("mab_mix_name_std", "IN", paste(unlist(lapply(private$.mabMixture, URLencode, reserved = TRUE)), collapse = ";"))),
            do.call(rbind, filters)
          )
        )
      } else {
        mabFilters <- NULL
      }

      nabMab <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "study",
        queryName = "NAbMAbWithMixMeta",
        colNameOpt = "fieldname",
        colFilter = mabFilters,
        method = "GET"
      )
      setDT(nabMab)

      private$.nabMab <- nabMab
    },

    .getMabs = function() {
      mabs <- labkey.executeSql(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "CDS",
        sql = paste0(
          "SELECT ",
          "     mabmetadata.mab_id, mabmetadata.mab_name_std, mabmetadata.mab_lanlid, mabmetadata.mab_hxb2_location, ",
          "     mabmetadata.mab_ab_binding_type, mabmetadata.mab_isotype, mabmetadata.mab_donorid, ",
          "     mabmetadata.mab_donor_species, mabmetadata.mab_donor_clade ",
          "FROM mabmetadata ",
          "INNER JOIN mabmix ON mabmetadata.mab_id = mabmix.mab_id ",
          "INNER JOIN mabmixmetadata on mabmix.mab_mix_id = mabmixmetadata.mab_mix_id ",
          "WHERE mab_mix_name_std IN('", paste0(unique(private$.nabMab$mab_mix_name_std), collapse = "', '"), "') "
        ),
        colNameOpt = "fieldname"
      )
      setDT(mabs)

      private$.mabs <- mabs
    },

    .getStudies = function() {
      studies <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "study",
        colNameOpt = "fieldname",
        colFilter = makeFilter(c("study_name", "IN", paste(unique(private$.nabMab$prot), collapse = ";"))),
        method = "GET"
      )
      setDT(studies)
      setnames(studies, "study_name", "prot")

      studyDocument <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "studydocument",
        colNameOpt = "fieldname",
        colFilter = makeFilter(c("prot", "IN", paste(unique(private$.nabMab$prot), collapse = ";"))),
        method = "GET"
      )
      setDT(studyDocument)

      studies <- merge(studies, studyDocument, by = "prot", all.x = TRUE)

      private$.studies <- unique(
        studies[, .(
          network,
          prot,
          grant_pi_name,
          investigator_name,
          primary_poc_name,
          primary_poc_email,
          description,
          type,
          species,
          access_level
        )]
      )
    },

    .getAssays = function() {
      assays <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "studyassay",
        colNameOpt = "fieldname",
        colFilter = makeFilter(
          c("prot", "IN", paste(unique(private$.nabMab$prot), collapse = ";")),
          c("assay_identifier", "IN", "NAB MAB")
        ),
        method = "GET"
      )
      setDT(assays)

      private$.assays <- assays[, container := NULL]
    },

    .getVariableDefinitions = function() {
      varInfo <- labkey.getQueryDetails(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "study",
        queryName = "NAbMAbWithMixMeta"
      )
      setDT(varInfo)
      setnames(varInfo, "fieldName", "field_name")

      private$.variableDefinitions <- varInfo[, .(field_name, caption, description)]
    }
  )
)
