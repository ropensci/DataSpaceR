#' The DataSpaceMab class
#'
#' @section Constructor:
#' \code{DataSpaceConnection$getMab()}
#'
#' @section Fields:
#' \describe{
#' }
#'
#' @section Methods:
#' \describe{
#' }
#' @seealso \code{\link{connectDS}} \code{\link{DataSpaceConnection}}
#' @examples
#' \dontrun{
#' }
#' @docType class
DataSpaceMab <- R6Class(
  classname = "DataSpaceMab",
  public = list(
    studyAndMabs        = data.table(),
    mabs                = data.table(),
    nabMab              = data.table(),
    metadata            = data.table(),
    studies             = data.table(),
    assays              = data.table(),
    variableDefinitions = data.table(),

    initialize = function(mab_mixture, filters, config) {
      private$.config <- config
      private$.filters <- filters

      if (length(filters) > 0) {
        filters <- lapply(names(filters), function(x) {
          Rlabkey::makeFilter(c(x, "IN", paste(unique(unlist(lapply(filters[[x]], URLencode, reserved = TRUE))), collapse = ";")))
        })
        mabFilters <- unique(
          rbind(
            Rlabkey::makeFilter(c("mab_mix_name_std", "IN", paste(unlist(lapply(mab_mixture, URLencode, reserved = TRUE)), collapse = ";"))),
            do.call(rbind, filters)
          )
        )
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
      self$nabMab <- nabMab

      self$studyAndMabs <- unique(data.table::copy(nabMab[,.(prot, mab_mix_id, mab_mix_label, mab_mix_name_std)]))

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
          "WHERE mab_mix_name_std IN('", paste0(unique(nabMab$mab_mix_name_std), collapse = "', '"), "') "
        ),
        colNameOpt = "fieldname"
      )
      setDT(mabs)
      self$mabs <- mabs
      
      self$metadata <- data.table(
        important_info = "By exporting data from the CAVD DataSpace, you agree to be bound by the Terms of Use available on the CAVD DataSpace sign-in page at https://dataspace.cavd.org/cds/CAVD/app.view?  nData included may have additional sharing restrictions; please refer to the Studies tab for details. Please notify the DataSpace team of any presentations or publications resulting from this data and remember to cite the CAVD DataSpace, as well as the grant and study investigators. Thank you!",
        export_date = Sys.time(),
        data_summary_level = "Neutralization curve details and titers by virus and mAb concentration.",
        filters_applied = list(list(mabFilters))
      )

      studies <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "study",
        colNameOpt = "fieldname",
        colFilter = Rlabkey::makeFilter(c("study_name", "IN", paste(unique(nabMab$prot), collapse = ";"))),
        method = "GET"
      )
      setDT(studies)
      data.table::setnames(studies, "study_name", "prot")
      studyDocument <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "studydocument",
        colNameOpt = "fieldname",
        colFilter = Rlabkey::makeFilter(c("prot", "IN", paste(unique(nabMab$prot), collapse = ";"))),
        method = "GET"
      )
      setDT(studyDocument)
      studies <- merge(studies,studyDocument, by = "prot")
      self$studies <- unique(
        studies[,.(
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

      assays <- labkey.selectRows(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "cds",
        queryName = "studyassay",
        colNameOpt = "fieldname",
        colFilter = Rlabkey::makeFilter(c("prot", "IN", paste(unique(nabMab$prot), collapse = ";")),
                                        c("assay_identifier", "IN", "NAB MAB")),
        method = "GET"
      )
      setDT(assays)
      self$assays <- assays[,container:=NULL]
      
      varInfo <- labkey.getQueryDetails(
        baseUrl = private$.config$labkeyUrlBase,
        folderPath = "/CAVD",
        schemaName = "study",
        queryName = "NAbMAbWithMixMeta"
      )
      setDT(varInfo)
      data.table::setnames(varInfo, "fieldName", "field_name")
      self$variableDefinitions <- varInfo[,.(field_name, caption, description)]
      
    },
    print = function() {
      cat("<DataSpaceMab>")
      cat("\n  URL:", private$.config$labkey.url.base)
      cat("\n  User:", private$.config$labkey.user.email)
      cat("\n  Summary:")
      cat("\n    -", length(unique(self$nabMab$prot)), "studies")
      cat("\n    -", length(unique(self$nabMab$mab_mix_name_std)), "mAb mixtures")
      cat("\n    -", length(unique(self$nabMab$neutralization_tier)), "neutralization tiers")
      cat("\n    -", length(unique(self$nabMab$clade)), "clades")
      cat("\n  Filters:")
      if (length(private$.filters) > 0) {
        lapply(names(private$.filters), function(x) {
          cat("\n    - ", x, ": ", paste(private$.filters[[x]], collapse = ", "), sep = "")
        })
      } else {
        cat(" NA")
      }
      cat(" \n")
    }
  ),
  active = list(),
  private = list(
    .config = list(),
    .filters = list()
  )
)
