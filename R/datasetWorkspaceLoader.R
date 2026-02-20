#' Load selected datasets into a combined workspace
#'
#' @param con database connection
#' @param selected_datasets character vector of dataset table names
#'
#' @return list with importedData and currentDatasetRowMap
#' @export
load_selected_datasets_workspace <- function(con, selected_datasets) {
  selected_datasets <- unique(as.character(selected_datasets))
  selected_datasets <- selected_datasets[!is.na(selected_datasets) & nzchar(selected_datasets)]
  if (is.null(con) || length(selected_datasets) == 0) {
    return(list(importedData = tibble::tibble(), currentDatasetRowMap = tibble::tibble()))
  }

  loaded_tbls <- list()
  for (dataset_name in selected_datasets) {
    tbl <- dplyr::tbl(con, dataset_name) %>%
      dplyr::collect() %>%
      dplyr::mutate_all(as.character)
    tbl <- ensure_rowid_column(
      data = tbl,
      table_name = dataset_name,
      require_unique = TRUE,
      allow_long = FALSE
    )
    tbl <- tbl %>% dplyr::mutate(`.__source_dataset` = dataset_name)
    loaded_tbls[[dataset_name]] <- tbl
  }
  imported_tbl <- dplyr::bind_rows(loaded_tbls) %>%
    dplyr::mutate(
      `.__source_rowid` = as.character(.data$rowid),
      rowid = as.character(seq_len(dplyr::n()))
    )

  row_map <- imported_tbl %>%
    dplyr::transmute(
      rowid = as.character(.data$rowid),
      dataset_name = as.character(.data$`.__source_dataset`),
      source_rowid = as.character(.data$`.__source_rowid`)
    )

  list(
    importedData = imported_tbl %>% dplyr::select(-tidyselect::any_of(c(".__source_dataset", ".__source_rowid"))),
    currentDatasetRowMap = row_map
  )
}

#' Load merged variable metadata across selected datasets
#'
#' @param con database connection
#' @param selected_datasets character vector of dataset table names
#'
#' @return character vector of variable names from metadata
#' @export
load_selected_dataset_metadata_variables <- function(con, selected_datasets) {
  selected_datasets <- unique(as.character(selected_datasets))
  selected_datasets <- selected_datasets[!is.na(selected_datasets) & nzchar(selected_datasets)]
  if (is.null(con) || length(selected_datasets) == 0) return(character())

  filenames <- paste0(selected_datasets, "_metadata")
  tblsmd <- list()
  for (tbl in filenames) {
    if (db_table_exists_safe(con, tbl)) {
      tblsmd[[tbl]] <- dplyr::tbl(con, tbl) %>%
        dplyr::collect() %>%
        dplyr::mutate_all(as.character)
    }
  }
  if (length(tblsmd) == 0) return(character())
  do.call(dplyr::bind_rows, tblsmd) %>%
    dplyr::distinct_all() %>%
    dplyr::filter(field == "variable") %>%
    dplyr::pull(value) %>%
    as.character()
}
