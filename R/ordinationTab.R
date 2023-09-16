
#' UI elements for ordination
#'
#' @return
#' @export
#'
#' @examples
ordinationTab = function(){tabPanel(title = "Ordination", icon = icon("equalizer", lib = "glyphicon"),
                  sidebarLayout(
                    sidebarPanel(
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

  output$pca.button <- renderUI({
    actionButton("runPCA", "Run PCA")
  })

  observeEvent(input$runPCA, {
    req(rvals$selectedData)
    rvals$pca = prcomp(rvals$selectedData[,rvals$chem])
    rvals$pcadf = dplyr::bind_cols(rvals$selectedData[,rvals$attrs],rvals$pca$x)
  })

  # Render PCA plot
  output$pca.plot <- renderPlot({
    req(rvals$pca)
    factoextra::fviz_pca_ind(
      rvals$pca,
      col.ind = "cos2",
      # Color by the quality of representation
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      label = 'none',
      repel = TRUE
    )     # Avoid text overlapping
  })

  # Render PCA Eigenvalue plot
  output$eigen.plot <- renderPlot({
    req(rvals$pca)
    factoextra::fviz_eig(rvals$pca)
  })

  # Render PCA Eigenvalue plot
  output$pca.el.plot <- renderPlot({
    req(rvals$pca)
    factoextra::fviz_pca_var(
      rvals$pca,
      col.var = "contrib",
      # Color by contributions to the PC
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      repel = TRUE
    )     # Avoid text overlapping
  })
}
