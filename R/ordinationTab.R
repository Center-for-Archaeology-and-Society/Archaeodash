
#' UI elements for ordination
#'
#' @return
#' @export
#'
#' @examples
ordinationTab = function(){
  tabPanel(title = "Ordination", icon = icon("equalizer", lib = "glyphicon"),
           fluidPage(
             fluidRow(column(6,
                             h1("PCA Results"))),
             fluidRow(
               column(6,plotOutput("pca.plot")),
               column(6,plotOutput("pca.el.plot"))),
             fluidRow(
               column(6,plotOutput("eigen.plot"))),
             fluidRow(column(6,
                             h1("CDA Results"))),
             fluidRow(column(6,
                             plotOutput("cda.plot")))
           ) # end fluidPage Ordination
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

  observeEvent(rvals$runPCA, {
    req(rvals$runPCA)
    req(rvals$selectedData)
    if(isTRUE(rvals$runPCA)){
      quietly({
      rvals$pca = prcomp(rvals$selectedData[,rvals$chem])
      rvals$pcadf = dplyr::bind_cols(rvals$selectedData[,rvals$attrs],rvals$pca$x)
      rvals$runPCA = F
      })
    }
  })

  observeEvent(rvals$runCDA, {
    req(rvals$runCDA)
    req(rvals$selectedData)
    if(isTRUE(rvals$runCDA)){
      quietly({
        cda = getCDA(df = rvals$selectedData, chem = rvals$chem, attrGroups = rvals$attrGroups)
        rvals$CDAdf = cda$CDAdf
        rvals$CDAmod = cda$mod
        rvals$runCDA = F
      })
    }
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

  # Render CDA plot
  output$cda.plot <- renderPlot({
    req(rvals$CDAmod)
    levels = nrow(rvals$CDAdf)
    cp = viridis::cividis(n = length(levels))
    xlim = c(min(rvals$CDAdf$Can1) * 1.25,max(rvals$CDAdf$Can1) * 1.25)
    ylim = c(min(rvals$CDAdf$Can2) * 1.25,max(rvals$CDAdf$Can2) * 1.25)
    heplots::heplot(rvals$CDAmod, col = cp, xlim = xlim, ylim = ylim)
  })
}
