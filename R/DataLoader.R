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
    n = nchar(credentials$res$username) + 11
    fn = input$file1$name %>% tools::file_path_sans_ext() %>% stringr::str_sub(1,64 - n)
    if(isTruthy(length(fn) > 0)){
      textInput("datasetName","Enter a name for the dataset", value = fn)
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

  output$columnsUI = renderUI({
    choices = names(rvals$data)
    excluded_defaults = c("rowid", "anid")
    selected = choices[!tolower(choices) %in% excluded_defaults]
    selectInput('loadcolumns',"Choose which columns should be included", choices = choices, selected = selected, multiple = T)
  })


  output$loadchemUI = renderUI({
    choices = names(rvals$data)
    dfNum = suppressWarnings(rvals$data %>% dplyr::mutate_all(as.numeric) %>%
                               janitor::remove_empty("cols"))
    excluded_defaults = c("rowid", "anid")
    selected = names(dfNum)[!tolower(names(dfNum)) %in% excluded_defaults]
    selectInput('loadchem',"Choose which columns are elements/predictor variables", choices = choices, selected = selected, multiple = T)
  })

  output$loadOptionsUI = renderUI({
    tagList(
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

      selected_cols = unique(c(input$loadcolumns, input$loadchem))
      data_loaded = rvals$data %>%
        dplyr::select(tidyselect::all_of(selected_cols)) %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(input$loadchem), ~ suppressWarnings(as.numeric(as.character(.)))))

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

      rvals$importedData = data_loaded
      rvals$selectedData = data_loaded
      rvals$chem = intersect(input$loadchem, names(data_loaded))
      rvals$initialChem = rvals$chem
      rvals$loadZeroAsNA = isTRUE(input$loadZeroAsNA)
      rvals$loadNegativeAsNA = isTRUE(input$loadNegativeAsNA)
      rvals$loadNAAsZero = isTRUE(input$loadNAAsZero)

      if(isTruthy(credentials$status) && !is.null(con)){
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
        DBI::dbWriteTable(conn = con, name = datasetname, value = data_loaded, row.names = F)
        DBI::dbWriteTable(conn = con, name = paste0(datasetname,"_metadata"), value = data_metadata, row.names = F)
        mynotification("Data Loaded")
      } else {
        mynotification("Data loaded locally")
      }
      removeModal()
    }, error = function(e){
      mynotification(paste("An error occurred. Please try again. Error:\n",e), type = "error")
    })
  })
}
