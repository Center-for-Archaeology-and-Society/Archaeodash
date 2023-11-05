

#' UI Elements for Saving and Exporting
#'
#' @return UI
#' @export
#'
#' @examples
#' saveexportTab()
saveexportTab = function() {
  div(class = "exportSection",
      br(),
      "Export selected data",
      uiOutput("exportFileUI"),
      radioButtons(
        inputId = "dataType",
        label = "Results type",
        choices = c("chemical", "PCA","Membership Probabilities"),
        selected = "chemical",
        inline = F
      ),
      textInput('ExportName', label = 'File name including the file extension (e.g., csv, xlsx)'),
      br(),
      downloadButton("Save", "Click here to save file", class = 'mybtn')
  )
}

#' Save and Export Server
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param rvals reactive values object
#'
#' @return server
#' @export
#'
#' @examples
#' saveExportServer(input,output,session,rvals)
saveExportServer = function(input, output, session, rvals) {

  # observeEvent(rvals$selectedData,{
  #   req(rvals$selectedData)
  #   shinyjs::show(id = "exportSection")
  # })

  output$exportFileUI = renderUI({
    req(rvals$selectedData)
    selectInput(inputId = 'fileDownload',
                label = "choose file to download",
                choices = unique(rvals$importedData$file),multiple = T)
  })

  output$Save <- downloadHandler(
    filename = function() {
      ifelse(
        isTRUE(stringr::str_detect(
          input$ExportName, stringr::fixed(".")
        )),
        input$ExportName,
        paste0(input$ExportName, ".xlsx")
      )
    },
    content = function(file) {
      if(input$dataType == "PCA"){
        data = rvals$pcaData
      } else if(input$dataType == "Membership Probabilities"){
        data = rvals$membershipProbs
      } else {
        data = rvals$selectedData
      }
      if(!is.null(input$fileDownload)){
        data = data %>%
          dplyr::filter(file %in% input$fileDownload)
      }
      rio::export(data,file)
  }
  )
}
