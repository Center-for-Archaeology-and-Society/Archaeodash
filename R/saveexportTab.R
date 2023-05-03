
#' UI Elements for Saving and Exporting
#'
#' @return
#' @export
#'
#' @examples
saveexportTab = function(){tabPanel(title = "Save & Export", icon = icon("download"),
                  sidebarLayout(
                    sidebarPanel(
                      chooseDFUI("se"),
                      br(),
                      subsetModUI("se"),
                      br(),
                      "Export selected attributes (including new/updated clusters/designations) and transformed/imputed elements",
                      textInput('ExportName', label = 'Type name for export (include file type (e.g., csv, xlsx)'),
                      br(),
                      downloadButton("Save","Click here to save file")
                    ), # end sidebarPanel

                    mainPanel(

                    ) # end mainPanel Save and Export
                  ) # end sidebarLayout Save and Export
) # end tabPanel "Save and Export"
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
saveExportServer = function(input,output,session,rvals){

  chooseDFServer("se",rvals)

  subsetModServer("se",rvals)

  output$Save <- downloadHandler(
    filename = function() {
      ifelse(isTRUE(stringr::str_detect(input$ExportName,stringr::fixed("."))),input$ExportName,paste0(input$ExportName,".xlsx"))
    },
    content = function(file) {
      rio::export(dplyr::bind_cols(rvals$df[[input$`se-selectedDF`]]$attrData, rvals$df[[input$`se-selectedDF`]]$chemicalData),
                  file)
    }
  )
}
