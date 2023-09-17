

#' UI Elements for Saving and Exporting
#'
#' @return
#' @export
#'
#' @examples
saveexportTab = function() {
  div(class = "exportSection",
      br(),
      "Export selected data",
      radioButtons(
        inputId = "dataType",
        label = "Results type",
        choices = c("chemical", "PCA","Membership Probabilities"),
        selected = "chemical",
        inline = F
      ),
      textInput('ExportName', label = 'File name including the file extension (e.g., csv, xlsx)'),
      br(),
      downloadButton("Save", "Click here to save file")
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

  # observeEvent(rvals$selectedData,{
  #   req(rvals$selectedData)
  #   shinyjs::show(id = "exportSection")
  # })

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

      rio::export(data,file)
  }
  )
}
