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
  if (!isTruthy(credentials$status) || is.null(con)) return(NULL)
  if (!app_require_packages("DBI", feature = "Saving current dataset")) {
    return(NULL)
  }
  tryCatch({
    ensure_core_rowids(rvals)
    dataset_name <- tryCatch(as.character(rvals$currentDatasetName[[1]]), error = function(e) "")
    if (!nzchar(dataset_name)) {
      dataset_name <- tryCatch(as.character(input$selectedDatasets[[1]]), error = function(e) "")
    }
    if (!nzchar(dataset_name) || !DBI::dbExistsTable(con, dataset_name)) return(NULL)

    saveTbl <- rvals$selectedData %>%
      dplyr::select(-tidyselect::any_of(c("transformation", "imputation", rvals$chem))) %>%
      dplyr::arrange(rowid)
    saveTbl <- dplyr::bind_cols(
      saveTbl,
      rvals$importedData %>%
        dplyr::filter(rowid %in% rvals$selectedData$rowid) %>%
        dplyr::arrange(rowid) %>%
        dplyr::select(tidyselect::any_of(rvals$chem))
    )

    meta_name <- paste0(dataset_name, "_metadata")
    old_meta <- tibble::tibble(field = character(), value = character())
    if (DBI::dbExistsTable(con, meta_name)) {
      old_meta <- dplyr::tbl(con, meta_name) %>%
        dplyr::collect() %>%
        dplyr::mutate_all(as.character)
    }
    old_dataset_label <- old_meta %>%
      dplyr::filter(field == "datasetName") %>%
      dplyr::pull(value)
    dataset_label <- if (length(old_dataset_label) > 0 && nzchar(old_dataset_label[[1]])) {
      as.character(old_dataset_label[[1]])
    } else {
      as.character(dataset_name)
    }
    old_created <- old_meta %>%
      dplyr::filter(field == "created") %>%
      dplyr::pull(value)
    created_label <- if (length(old_created) > 0 && nzchar(old_created[[1]])) {
      as.character(old_created[[1]])
    } else {
      as.character(as.Date(Sys.time()))
    }
    data_metadata <- tibble::tibble(
      field = c("datasetName", "created", rep("variable", length(rvals$chem))),
      value = c(dataset_label, created_label, rvals$chem)
    )

    DBI::dbWriteTable(
      con,
      dataset_name,
      saveTbl,
      overwrite = TRUE,
      row.names = FALSE
    )
    DBI::dbWriteTable(
      conn = con,
      name = meta_name,
      value = data_metadata,
      row.names = FALSE,
      overwrite = TRUE
    )
    mynotification(paste0("Autosaved to ", dataset_name))
  }, error = function(e) {
    mynotification(paste("Error saving data to database\n", e), type = "error")
  })
}
