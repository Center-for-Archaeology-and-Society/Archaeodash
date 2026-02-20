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
  if (!shiny::isTruthy(credentials$status) || is.null(con)) return(NULL)
  if (!app_require_packages("DBI", feature = "Saving current dataset")) {
    return(NULL)
  }
  tryCatch({
    ensure_core_rowids(rvals)
    dataset_names <- tryCatch(as.character(rvals$currentDatasetName), error = function(e) character())
    dataset_names <- dataset_names[!is.na(dataset_names) & nzchar(dataset_names)]
    if (length(dataset_names) == 0) {
      fallback_name <- tryCatch(as.character(input$selectedDatasets[[1]]), error = function(e) "")
      if (nzchar(fallback_name)) dataset_names <- fallback_name
    }
    if (length(dataset_names) == 0) return(NULL)

    saveTbl <- rvals$importedData %>%
      dplyr::select(-tidyselect::any_of(c("transformation", "imputation"))) %>%
      dplyr::arrange(rowid)
    saveTbl$rowid <- as.character(saveTbl$rowid)

    source_map <- tryCatch(rvals$currentDatasetRowMap, error = function(e) NULL)
    has_valid_map <- inherits(source_map, "data.frame") &&
      all(c("rowid", "dataset_name", "source_rowid") %in% names(source_map))

    write_dataset <- function(dataset_name, data_tbl) {
      if (!nzchar(dataset_name) || !db_table_exists_safe(con, dataset_name)) return(FALSE)

      meta_name <- paste0(dataset_name, "_metadata")
      old_meta <- tibble::tibble(field = character(), value = character())
      if (db_table_exists_safe(con, meta_name)) {
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
      chem_cols <- tryCatch(as.character(rvals$chem), error = function(e) character())
      chem_cols <- chem_cols[chem_cols %in% names(data_tbl)]
      data_metadata <- tibble::tibble(
        field = c("datasetName", "created", rep("variable", length(chem_cols))),
        value = c(dataset_label, created_label, chem_cols)
      )

      ok_data <- db_write_table_safe(
        con = con,
        table_name = dataset_name,
        value = data_tbl,
        row.names = FALSE,
        overwrite = TRUE,
        context = "autosaving current dataset"
      )
      ok_meta <- db_write_table_safe(
        con = con,
        table_name = meta_name,
        value = data_metadata,
        row.names = FALSE,
        overwrite = TRUE,
        context = "autosaving dataset metadata"
      )
      isTRUE(ok_data) && isTRUE(ok_meta)
    }

    if (has_valid_map) {
      map_tbl <- source_map %>%
        dplyr::transmute(
          rowid = as.character(.data$rowid),
          dataset_name = as.character(.data$dataset_name),
          source_rowid = as.character(.data$source_rowid)
        ) %>%
        dplyr::filter(!is.na(.data$rowid), nzchar(.data$rowid), !is.na(.data$dataset_name), nzchar(.data$dataset_name))

      merged_tbl <- saveTbl %>%
        dplyr::left_join(map_tbl, by = "rowid")
      target_sets <- unique(as.character(merged_tbl$dataset_name))
      target_sets <- target_sets[!is.na(target_sets) & nzchar(target_sets)]

      if (length(target_sets) == 0) return(NULL)

      for (target_dataset in target_sets) {
        ds_tbl <- merged_tbl %>%
          dplyr::filter(.data$dataset_name == .env$target_dataset) %>%
          dplyr::mutate(rowid = .data$source_rowid) %>%
          dplyr::select(-tidyselect::any_of(c("dataset_name", "source_rowid")))

        if (!write_dataset(target_dataset, ds_tbl)) {
          mynotification(paste0("Unable to autosave to ", target_dataset), type = "error")
          return(NULL)
        }
      }
      mynotification(paste0("Autosaved to source datasets: ", paste(target_sets, collapse = ", ")))
      return(NULL)
    }

    dataset_name <- dataset_names[[1]]
    if (!write_dataset(dataset_name, saveTbl)) return(NULL)
    mynotification(paste0("Autosaved to ", dataset_name))
  }, error = function(e) {
    mynotification(paste("Error saving data to database\n", e), type = "error")
  })
}
