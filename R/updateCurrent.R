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
      build_current_transform_alias_tbl <- function(transformations, active_name, dataset_key = "") {
        if (is.null(transformations) || length(transformations) == 0) {
          return(tibble::tibble(
            alias = character(),
            transformation_name = character(),
            created = character(),
            is_active = logical(),
            dataset_key = character()
          ))
        }
        tx_names <- sort(unique(names(transformations)))
        tx_names <- tx_names[nzchar(tx_names)]
        if (length(tx_names) == 0) {
          return(tibble::tibble(
            alias = character(),
            transformation_name = character(),
            created = character(),
            is_active = logical(),
            dataset_key = character()
          ))
        }
        alias_vals <- paste0("t", seq_along(tx_names))
        created_vals <- vapply(tx_names, function(nm) {
          snap <- transformations[[nm]]
          if (is.null(snap$created) || length(snap$created) == 0 || is.na(snap$created[[1]])) "" else as.character(snap$created[[1]])
        }, FUN.VALUE = character(1))
        tibble::tibble(
          alias = alias_vals,
          transformation_name = tx_names,
          created = created_vals,
          is_active = tx_names == safe_chr(active_name),
          dataset_key = as.character(dataset_key)
        )
      }
      active_name <- safe_chr(rvals$activeTransformation)
      alias_tbl <- build_current_transform_alias_tbl(
        transformations = rvals$transformations,
        active_name = active_name,
        dataset_key = safe_chr(rvals$currentDatasetKey)
      )
      active_alias <- ""
      if (nrow(alias_tbl) > 0) {
        active_alias_rows <- alias_tbl$alias[alias_tbl$is_active | alias_tbl$transformation_name == active_name]
        if (length(active_alias_rows) > 0) active_alias <- as.character(active_alias_rows[[1]])
      }
      alias_table_name <- paste0(credentials$res$username, "_current_transformations")
      static_fields <- c(
        "datasetName",
        "created",
        "attrGroup",
        "transformationName",
        "transformationMethod",
        "imputationMethod",
        "transformationAliasTable",
        "transformationCount",
        "activeTransformationAlias",
        "currentDatasetKey"
      )
      static_values <- c(
        "current",
        as.character(as.Date(Sys.time())),
        safe_chr(rvals$attrGroups),
        active_name,
        safe_chr(rvals$transform.method),
        safe_chr(rvals$impute.method),
        alias_table_name,
        as.character(nrow(alias_tbl)),
        active_alias,
        safe_chr(rvals$currentDatasetKey)
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
      DBI::dbWriteTable(
        conn = con,
        name = alias_table_name,
        value = alias_tbl,
        row.names = FALSE,
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
