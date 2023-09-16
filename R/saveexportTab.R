

#' UI Elements for Saving and Exporting
#'
#' @return
#' @export
#'
#' @examples
saveexportTab = function() {
  shinyjs::hidden(
  div(class = "exportSection",
    br(),
    "Export selected data",
    radioButtons(
      inputId = "dataType",
      label = "Results type",
      choices = c("chemical", "PCA"),
      selected = "chemical",
      inline = F
    ),
    textInput('ExportName', label = 'File name including the file extension (e.g., csv, xlsx)'),
    br(),
    downloadButton("Save", "Click here to save file")
  )
  )
}

#' Save and Export Server
#'
#' @param input
#' @param output
#' @param session
#' @param rvals
#'
#' @return
#' @export
#'
#' @examples
saveExportServer = function(input, output, session, rvals) {

  observeEvent(rvals$selectedData,{
    req(rvals$selectedData)
    shinyjs::show(id = "exportSection")
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
      rio::export(ifelse(input$dataType == "PCA",rvals$pcaData,rvals$selectedData),file)
    }
  )
}
