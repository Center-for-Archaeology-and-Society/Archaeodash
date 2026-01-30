#' dataLoader
#'
#' @param filename
#'
#' @return data
#' @export
#'
#' @examples
#' dataLoader("data.csv")
dataLoader <- function(filename) {
  data <- rio::import(filename, setclass = "tibble") %>%
    dplyr::mutate_all(as.character) %>%
    janitor::clean_names(case = "none") %>%
    dplyr::select(-tidyselect::any_of("rowid")) %>%
    tibble::rowid_to_column()
  return(data)
}

#' dataLoaderUI
#'
#' @return NULL
#' @export
#'
#' @examples
#' dataLoaderUI()
dataLoaderUI <- function() {
  showModal(modalDialog(
    title = "Data Loader",
    uiOutput("datasetNameUI"),
    div(textOutput("notification"), style = "color: red; font-size: 16px; padding = 10px;"),
    uiOutput("columnsUI"),
    uiOutput("loadchemUI"),
    footer = tagList(modalButton("cancel"), actionButton("loadData", "load data")),
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
#' dataLoaderServer(input, output, session, credentials, con)
dataLoaderServer <- function(rvals, input, output, session, credentials, con) {
  output$datasetNameUI <- renderUI({
    # mysql table names have a 64 character limit
    n <- nchar(credentials$res$username) + 11
    fn <- input$file1$name %>%
      tools::file_path_sans_ext() %>%
      stringr::str_sub(1, 64 - n)
    if (isTruthy(length(fn) > 0)) {
      textInput("datasetName", "Enter a name for the dataset", value = fn)
    } else {
      textInput("datasetName", "Enter a name for the dataset")
    }
  })

  output$notification <- renderText({
    req(input$datasetName)
    tblList <- DBI::dbListTables(con)
    datasetname <- paste0(credentials$res$username, "_", input$datasetName) %>%
      janitor::make_clean_names()
    if (isTruthy(datasetname %in% tblList) && input$datasetName %>% length() > 0) {
      "This dataset already exists. Please choose a different name."
    } else if (input$datasetName %>% length() == 0) {
      "Please enter a name for the dataset."
    } else {
      NULL
    }
  })

  output$columnsUI <- renderUI({
    choices <- names(rvals$data)
    selectInput("loadcolumns", "Choose which columns should be included", choices = choices, selected = choices, multiple = T)
  })


  output$loadchemUI <- renderUI({
    choices <- names(rvals$data)
    dfNum <- suppressWarnings(rvals$data %>% dplyr::mutate_all(as.numeric) %>%
      janitor::remove_empty("cols"))
    selected <- names(dfNum)
    selectInput("loadchem", "Choose which columns are elements/predictor variables", choices = choices, selected = selected, multiple = T)
  })

  observeEvent(input$loadData, {
    tryCatch(
      {
        datasetname <- paste0(credentials$res$username, "_", input$datasetName) %>%
          janitor::make_clean_names()
        if (nchar(datasetname) > 53) {
          mynotification("Error: datasetname is too long. Please try again.", type = "error")
        } else {
          removeModal()
          data_metadata <- tibble::tibble(
            field = c("datasetName", "created", rep("variable", length(input$loadchem))),
            value = c(input$datasetName, as.character(as.Date(Sys.time())), input$loadchem)
          )
          tblList <- DBI::dbListTables(con)
          if (isTruthy(datasetname %in% tblList) || input$datasetName %>% length() == 0) {
            mynotification("This dataset already exists. Please choose a different name.", type = "error")
          } else {
            DBI::dbWriteTable(conn = con, name = datasetname, value = rvals$data %>% dplyr::select(tidyselect::all_of(c(input$loadcolumns, input$loadchem))), row.names = F)
            DBI::dbWriteTable(conn = con, name = paste0(datasetname, "_metadata"), value = data_metadata, row.names = F)
            mynotification("Data Loaded")
          }
        }
      },
      error = function(e) {
        mynotification(paste("An error occurred. Please try again. Error:\n", e), type = "error")
      }
    )
  })
}
