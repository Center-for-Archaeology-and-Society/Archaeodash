#' Save current table to database
#'
#' @param rvals reactive values object
#' @param con database connection
#' @param credentials reactive values object
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return NULL
#' @export
#'
#' @examples
#' updateCurrent(rvals,con,credentials,input,output,session)
updateCurrent = function(rvals,
                         con,
                         credentials,
                         input,
                         output,
  session) {
  if (isTruthy(credentials$status)) {
    if (!app_require_packages("DBI", feature = "Saving current dataset")) {
      return(NULL)
    }
    tryCatch({
      print(head(rvals$selectedData))
      print(head(rvals$importedData))
      print(rvals$chem)
      print(credentials$res)
      message("saving data to database")
      saveTbl = rvals$selectedData %>% dplyr::select(-tidyselect::any_of(c("transformation", "imputation",rvals$chem))) %>%
        dplyr::arrange(rowid)
    print(1)
      saveTbl = dplyr::bind_cols(saveTbl,rvals$importedData %>%
        dplyr::filter(rowid %in% rvals$selectedData$rowid) %>%
        dplyr::arrange(rowid) %>%
        dplyr::select(tidyselect::any_of(rvals$chem))
      )
      print(2)

      safe_chr <- function(x) {
        if (is.null(x) || length(x) == 0 || is.na(x[[1]])) "" else as.character(x[[1]])
      }
      static_fields <- c(
        "datasetName",
        "created",
        "attrGroup",
        "transformationName",
        "transformationMethod",
        "imputationMethod"
      )
      static_values <- c(
        "current",
        as.character(as.Date(Sys.time())),
        safe_chr(rvals$attrGroups),
        safe_chr(rvals$activeTransformation),
        safe_chr(rvals$transform.method),
        safe_chr(rvals$impute.method)
      )
      data_metadata = tibble::tibble(
        field = c(static_fields, rep("variable", length(rvals$chem))),
        value = c(static_values, rvals$chem)
      )
print(3)
      DBI::dbWriteTable(
        con,
        paste0(credentials$res$username, "_current"),
        saveTbl,
        overwrite = TRUE,
        row.names = F
      )
      print(4)
      DBI::dbWriteTable(
        conn = con,
        name = paste0(credentials$res$username, "_current_metadata"),
        value = data_metadata,
        row.names = F,
        overwrite = TRUE
      )
print(5)
      updateSelectInput(
        session = session,
        inputId = "selectedDatasets",
        selected = paste0(credentials$res$username, "_current")
      )
      mynotification("table saved as current")
    }, error = function(e)
      mynotification(paste("Error saving data to database\n", e), type = "error"))
  }
}
