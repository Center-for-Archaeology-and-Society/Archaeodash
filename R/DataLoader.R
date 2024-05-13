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
    dplyr::mutate_all(as.character)
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
    textInput("datasetName","Dataset Name",value = "dataset1"),
    footer = tagList(modalButton("cancel"), actionButton("loadData","load data")),
    easyClose = T
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
dataLoaderServer = function(data, input,output,session, credentials, con){
  observeEvent(input$loadData,{
    removeModal()
    DBI::dbWriteTable(conn = con, name = paste0(credentials$res$username,"_",input$datasetName), value = data)
    mynotification("Data Loaded")
  })
}
