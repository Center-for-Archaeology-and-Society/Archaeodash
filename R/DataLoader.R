#' dataLoader
#'
#' @param filename
#'
#' @return data
#' @export
#'
#' @examples
#' dataLoader("data.csv")
dataLoader = function(filename){
  data = rio::import(filename, setclass = 'tibble') %>%
    dplyr::mutate_all(as.character) %>%
    janitor::clean_names(case = 'none') %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::select(-tidyselect::any_of("rowid")) %>%
    tibble::rowid_to_column()
  return(data)
}

# Common INAA element columns derived from inst/app/INAA_test.csv.
common_inaa_elements <- c(
  "as", "la", "lu", "nd", "sm", "u", "yb", "ce", "co", "cr", "cs",
  "eu", "fe", "hf", "ni", "rb", "sb", "sc", "sr", "ta", "tb", "th",
  "zn", "zr", "al", "ba", "ca", "dy", "k", "mn", "na", "ti", "v"
)

default_chem_columns <- function(column_names) {
  normalized_names <- tolower(column_names)
  matched_common <- column_names[normalized_names %in% common_inaa_elements]

  if (length(matched_common) > 0) {
    return(matched_common)
  }

  excluded_defaults <- c("rowid", "anid")
  column_names[!normalized_names %in% excluded_defaults]
}

default_id_column <- function(column_names) {
  matches <- column_names[tolower(column_names) == "anid"]
  if (length(matches) > 0) {
    return(matches[[1]])
  }
  ""
}

resolve_id_column <- function(selected_id_column, column_names) {
  if (is.null(selected_id_column) || !nzchar(selected_id_column)) {
    return("rowid")
  }
  if (!(selected_id_column %in% column_names)) {
    return("rowid")
  }
  selected_id_column
}

replace_non_element_blanks <- function(data, chem_cols, blank_label = "[blank]") {
  if (!inherits(data, "data.frame")) return(data)
  non_element_cols <- setdiff(names(data), c("rowid", chem_cols))
  if (length(non_element_cols) == 0) return(data)
  data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(non_element_cols),
        ~ {
          values <- as.character(.)
          values[is.na(values) | !nzchar(trimws(values))] <- blank_label
          values
        }
      )
    )
}

merge_loaded_data <- function(existing_data, incoming_data, mode = c("replace", "add")) {
  mode <- match.arg(mode)
  if (!inherits(incoming_data, "data.frame")) return(incoming_data)
  if (identical(mode, "replace") || !inherits(existing_data, "data.frame") || nrow(existing_data) == 0) {
    out <- incoming_data
    out <- ensure_rowid_column(
      data = out,
      table_name = "loadedData",
      require_unique = TRUE,
      allow_long = FALSE
    )
    return(out)
  }

  existing_tbl <- existing_data %>% dplyr::select(-tidyselect::any_of("rowid"))
  incoming_tbl <- incoming_data %>% dplyr::select(-tidyselect::any_of("rowid"))
  out <- dplyr::bind_rows(existing_tbl, incoming_tbl)
  out <- ensure_rowid_column(
    data = out,
    table_name = "loadedData",
    require_unique = TRUE,
    allow_long = FALSE
  )
  out
}

#' dataLoaderUI
#'
#' @return NULL
#' @export
#'
#' @examples
#' dataLoaderUI()
dataLoaderUI = function(){
  showModal(modalDialog(
    title = "Data Loader",
    uiOutput("datasetNameUI"),
    div(textOutput("notification"), style = "color: red; font-size: 16px; padding = 10px;"),
    uiOutput("idColumnUI"),
    uiOutput("columnsUI"),
    uiOutput("loadchemUI"),
    uiOutput("loadOptionsUI"),
    footer = tagList(modalButton("cancel"), actionButton("loadData","load data")),
    easyClose = F
  ))
}

#' dataLoaderServer
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#'
#' @return NULL
#' @export
#'
#' @examples
#' dataLoaderServer(input,output,session, credentials, con)
dataLoaderServer = function(rvals, input,output,session, credentials, con){

  output$datasetNameUI = renderUI({
    if(!isTruthy(credentials$status)){
      return(NULL)
    }
    #mysql table names have a 64 character limit
    username_prefix_length = nchar(credentials$res$username) + 11
    suggested_name = input$file1$name %>% tools::file_path_sans_ext() %>% stringr::str_sub(1,64 - username_prefix_length)
    if(isTruthy(length(suggested_name) > 0)){
      textInput("datasetName","Enter a name for the dataset", value = suggested_name)
    } else {
      textInput("datasetName","Enter a name for the dataset")
    }
  })

  output$notification = renderText({
    if(!isTruthy(credentials$status) || is.null(con)){
      return(NULL)
    }
    req(input$datasetName)
    tblList = DBI::dbListTables(con)
    datasetname = paste0(credentials$res$username,"_",input$datasetName) %>%
      janitor::make_clean_names()
    if(isTruthy(datasetname %in% tblList) && input$datasetName %>% length() > 0){
      "This dataset already exists. Please choose a different name."
    } else if(input$datasetName %>% length() == 0){
      "Please enter a name for the dataset."
    } else {
      NULL
    }
  })

  output$idColumnUI = renderUI({
    choices = names(rvals$data)
    selected = default_id_column(choices)
    selectInput(
      "loadIDColumn",
      "Choose ID column (defaults to ANID if present; otherwise rowid)",
      choices = c("Use rowid (default)" = "", choices),
      selected = selected,
      multiple = FALSE
    )
  })

  output$columnsUI = renderUI({
    choices = names(rvals$data)
    excluded_defaults = c("rowid", "anid")
    selected = choices[!tolower(choices) %in% excluded_defaults]
    selectInput('loadcolumns',"Choose which columns should be included", choices = choices, selected = selected, multiple = T)
  })


  output$loadchemUI = renderUI({
    choices = names(rvals$data)
    numeric_columns_df = suppressWarnings(rvals$data %>% dplyr::mutate_all(as.numeric) %>%
                                            janitor::remove_empty("cols"))
    selected = default_chem_columns(names(numeric_columns_df))
    selectInput('loadchem',"Choose which columns are elements/predictor variables", choices = choices, selected = selected, multiple = T)
  })

  output$loadOptionsUI = renderUI({
    has_existing <- inherits(rvals$importedData, "data.frame") && nrow(rvals$importedData) > 0
    tagList(
      radioButtons(
        "loadMode",
        "When loading this file",
        choices = c("Replace existing workspace data" = "replace", "Add rows to existing workspace data" = "add"),
        selected = if (isTRUE(has_existing)) "add" else "replace",
        inline = FALSE
      ),
      checkboxInput("loadBlankNonElement", "Replace empty/NA non-element fields with [blank]", value = TRUE),
      checkboxInput("loadZeroAsNA", "Treat zero values as NA", value = FALSE),
      checkboxInput("loadNegativeAsNA", "Treat negative values as NA", value = FALSE),
      checkboxInput("loadNAAsZero", "Replace NA values with 0", value = FALSE)
    )
  })

  observeEvent(input$loadData,{
    tryCatch({
      req(rvals$data)
      req(input$loadcolumns)
      req(input$loadchem)

      id_column = resolve_id_column(input$loadIDColumn, names(rvals$data))
      selected_cols = unique(c("rowid", input$loadcolumns, input$loadchem, id_column))
      selected_cols = intersect(selected_cols, names(rvals$data))
      data_loaded = rvals$data %>%
        dplyr::select(tidyselect::all_of(selected_cols)) %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(input$loadchem), ~ suppressWarnings(as.numeric(as.character(.)))))
      data_loaded <- ensure_rowid_column(
        data = data_loaded,
        table_name = "loadedData",
        require_unique = TRUE,
        allow_long = FALSE
      )

      if(isTRUE(input$loadZeroAsNA)){
        data_loaded = data_loaded %>%
          dplyr::mutate(dplyr::across(tidyselect::all_of(input$loadchem), ~ dplyr::na_if(., 0)))
      }
      if(isTRUE(input$loadNegativeAsNA)){
        data_loaded = data_loaded %>%
          dplyr::mutate(dplyr::across(tidyselect::all_of(input$loadchem), ~ dplyr::if_else(. < 0, NA_real_, .)))
      }
      if(isTRUE(input$loadNAAsZero)){
        data_loaded = data_loaded %>%
          dplyr::mutate(dplyr::across(tidyselect::all_of(input$loadchem), ~ tidyr::replace_na(., 0)))
      }
      if (isTRUE(input$loadBlankNonElement)) {
        data_loaded <- replace_non_element_blanks(data_loaded, chem_cols = input$loadchem)
      }

      load_mode <- if (isTruthy(input$loadMode)) as.character(input$loadMode[[1]]) else "replace"
      data_loaded <- merge_loaded_data(
        existing_data = rvals$importedData,
        incoming_data = data_loaded,
        mode = load_mode
      )

      rvals$importedData = data_loaded
      rvals$selectedData = data_loaded
      ensure_core_rowids(rvals)
      prior_chem <- tryCatch(as.character(rvals$chem), error = function(e) character())
      if (identical(load_mode, "add")) {
        rvals$chem = intersect(unique(c(prior_chem, as.character(input$loadchem))), names(data_loaded))
      } else {
        rvals$chem = intersect(input$loadchem, names(data_loaded))
      }
      rvals$initialChem = rvals$chem
      rvals$loadZeroAsNA = isTRUE(input$loadZeroAsNA)
      rvals$loadNegativeAsNA = isTRUE(input$loadNegativeAsNA)
      rvals$loadNAAsZero = isTRUE(input$loadNAAsZero)
      rvals$loadBlankNonElement = isTRUE(input$loadBlankNonElement)
      rvals$currentDatasetRowMap <- NULL

      if(isTruthy(credentials$status) && !is.null(con)){
        if (!app_require_packages("DBI", feature = "Saving uploaded datasets to database")) {
          return(NULL)
        }
        datasetname = paste0(credentials$res$username,"_",input$datasetName) %>%
          janitor::make_clean_names()
        if(nchar(datasetname) > 53){
          mynotification("Error: datasetname is too long. Please try again.", type = "error")
          return(NULL)
        }
        data_metadata = tibble::tibble(
          field = c("datasetName","created", rep("variable",length(rvals$chem))),
          value = c(input$datasetName,as.character(as.Date(Sys.time())),rvals$chem)
        )
        tblList = DBI::dbListTables(con)
        if(isTruthy(datasetname %in% tblList) || input$datasetName %>% length() == 0){
          mynotification("This dataset already exists. Please choose a different name.", type = "error")
          return(NULL)
        }
        ok_data <- db_write_table_safe(con, datasetname, data_loaded, row.names = FALSE, context = "saving uploaded dataset")
        ok_meta <- db_write_table_safe(con, paste0(datasetname, "_metadata"), data_metadata, row.names = FALSE, context = "saving uploaded dataset metadata")
        if (!isTRUE(ok_data) || !isTRUE(ok_meta)) {
          return(NULL)
        }
        username <- as.character(credentials$res$username[[1]])
        pref_tbl <- paste0(username, "_preferences")
        prefs <- if (db_table_exists_safe(con, pref_tbl)) {
          tryCatch(
            dplyr::tbl(con, pref_tbl) %>% dplyr::collect() %>% dplyr::mutate_all(as.character),
            error = function(e) tibble::tibble(field = character(), value = character())
          )
        } else {
          tibble::tibble(field = character(), value = character())
        }
        if (!all(c("field", "value") %in% names(prefs))) {
          prefs <- tibble::tibble(field = character(), value = character())
        }
        prefs <- prefs %>%
          dplyr::transmute(field = as.character(.data$field), value = as.character(.data$value)) %>%
          dplyr::filter(.data$field != "lastOpenedDataset") %>%
          dplyr::bind_rows(tibble::tibble(field = "lastOpenedDataset", value = datasetname))
        ok_pref <- db_write_table_safe(
          con = con,
          table_name = pref_tbl,
          value = prefs,
          row.names = FALSE,
          overwrite = TRUE,
          context = "saving user dataset preference"
        )
        if (!isTRUE(ok_pref)) {
          return(NULL)
        }
        rvals$currentDatasetName <- datasetname
        rvals$currentDatasetKey <- build_dataset_key(datasetname)
        mynotification("Data Loaded")
      } else {
        rvals$currentDatasetName <- "local_upload"
        rvals$currentDatasetKey <- build_dataset_key(paste0("local_upload_", format(Sys.time(), "%Y%m%d_%H%M%S")))
        mynotification("Data loaded locally")
      }
      removeModal()
    }, error = function(e){
      mynotification(paste("An error occurred. Please try again. Error:\n",e), type = "error")
    })
  })
}
