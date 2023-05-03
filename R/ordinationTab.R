
#' UI elements for ordination
#'
#' @return
#' @export
#'
#' @examples
ordinationTab = function(){tabPanel(title = "Ordination", icon = icon("equalizer", lib = "glyphicon"),
                  sidebarLayout(
                    sidebarPanel(
                      chooseDFUI("ot"),
                      br(),
                      subsetModUI("ot"),
                      br(),
                      uiOutput("chem.pca"),
                      uiOutput("pca.button")
                    ), # end sidebarPanel

                    mainPanel(
                      fluidRow(
                        column(6,plotOutput("pca.plot")),
                        column(6,plotOutput("pca.el.plot"))),
                      fluidRow(
                        column(6,plotOutput("eigen.plot")))
                    ) # end mainPanel Ordination
                  ) # end sidebarLayout Ordination
)
}

#' Ordination Server
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
ordinationServer = function(input,output,session,rvals){

  chooseDFServer("ot",rvals)

  subsetModServer("ot",rvals)

  # Render multi-select lookup for choosing chemical concentration columns to include in
  # Principal Components Analysis
  output$chem.pca <- renderUI({
    items.all <- names(rvals$df[[input$`ot-selectedDF`]]$chemicalData)
    names(items.all) = items.all
    selectInput(
      "chem.pca.sel",
      "Select transformed elements to include in PCA:",
      items.all,
      multiple = TRUE,
      selected = items.all
    )
  })

  output$pca.button <- renderUI({
    actionButton("runPCA", "Run PCA and Save Results")
  })

  observeEvent(input$runPCA, {
    req(rvals$df[[input$`ot-selectedDF`]]$chemicalData)
    req(input$chem.pca.sel)
    rvals$df[[input$`ot-selectedDF`]]$pcaResults = prcomp(rvals$df[[input$`ot-selectedDF`]]$chemicalData[input$chem.pca.sel])
  })

  # Render PCA plot
  output$pca.plot <- renderPlot({
    req(rvals$df[[input$`ot-selectedDF`]]$pcaResults)
    factoextra::fviz_pca_ind(
      rvals$df[[input$`ot-selectedDF`]]$pcaResults,
      col.ind = "cos2",
      # Color by the quality of representation
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      label = 'none',
      repel = TRUE
    )     # Avoid text overlapping
  })

  # Render PCA Eigenvalue plot
  output$eigen.plot <- renderPlot({
    req(rvals$df[[input$`ot-selectedDF`]]$pcaResults)
    factoextra::fviz_eig(rvals$df[[input$`ot-selectedDF`]]$pcaResults)
  })

  # Render PCA Eigenvalue plot
  output$pca.el.plot <- renderPlot({
    req(rvals$df[[input$`ot-selectedDF`]]$pcaResults)
    factoextra::fviz_pca_var(
      rvals$df[[input$`ot-selectedDF`]]$pcaResults,
      col.var = "contrib",
      # Color by contributions to the PC
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      repel = TRUE
    )     # Avoid text overlapping
  })
}
