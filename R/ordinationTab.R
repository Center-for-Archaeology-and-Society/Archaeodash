
#' UI elements for ordination
#'
#' @return UI
#' @export
#'
#' @examples
#' ordinationTab()
ordinationTab = function(){
  tabPanel(title = "Ordination", icon = icon("equalizer", lib = "glyphicon"),
           fluidPage(
             fluidRow(column(6,
                             h1("PCA Results"))),
             fluidRow(
               column(6,plotOutput("pca.plot")),
               column(6,plotOutput("pca.el.plot"))),
             fluidRow(
               column(6,plotOutput("eigen.plot")),
             column(6,tableOutput("contribTbl"))),
             fluidRow(column(6,
                             h1("LDA Results"))),
             fluidRow(column(6,
                             plotOutput("lda.plot")))
           ) # end fluidPage Ordination
  )
}

#' Ordination Server
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
#' ordinationServer(input,output,session,rvals)
ordinationServer = function(input,output,session,rvals){

  observeEvent(rvals$runPCA, {
    req(rvals$runPCA)
    req(rvals$selectedData)
    message("running PCA")
    quietly(label = "running PCA",{
      if(isTRUE(rvals$runPCA)){
        rvals$pca = prcomp(rvals$selectedData[,rvals$chem])
        rvals$pcadf = dplyr::bind_cols(rvals$selectedData[,rvals$attrs],rvals$pca$x)
        rvals$runPCA = F
      }
    })
  })

  observeEvent(rvals$runLDA, {
    req(rvals$runLDA)
    req(rvals$selectedData)
    message("running LDA")
    quietly(label = 'running LDA',{
      if(isTRUE(rvals$runLDA)){
        lda = tryCatch(getLDA(df = rvals$selectedData, chem = rvals$chem, attrGroups = rvals$attrGroups),error = function(e) return(list(LDAdf = tibble::tibble(), mod = NULL)))
        rvals$LDAdf = lda$LDAdf
        rvals$LDAmod = lda$mod
        rvals$runLDA = F
      }
    })
  })

  # Render PCA plot
  output$pca.plot <- renderPlot({
    req(rvals$pca)
    message("rendering PCA plot")
    quietly(label = 'PCA plot',{
      factoextra::fviz_pca_ind(
        rvals$pca,
        col.ind = "cos2",
        # Color by the quality of representation
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        label = 'none',
        repel = TRUE
      )     # Avoid text overlapping
    })
  })

  # Render PCA Eigenvalue plot
  output$eigen.plot <- renderPlot({
    req(rvals$pca)
    message("rendering eigenvalue plot")
    quietly(label = "PCA plot 2",{
      factoextra::fviz_eig(rvals$pca)
    })
  })

  # Render PCA Eigenvalue plot
  output$pca.el.plot <- renderPlot({
    req(rvals$pca)
    quietly(label = "PCA plot 3",{
      factoextra::fviz_pca_var(
        rvals$pca,
        col.var = "contrib",
        # Color by contributions to the PC
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        repel = TRUE
      )     # Avoid text overlapping
    })
  })

  # Render PCA contribution table
  output$contribTbl = renderTable({
    req(rvals$pca)
    message("rendering contribution table")
    rowSums(factoextra::get_pca_var(rvals$pca)$contrib[,1:4]) %>%
      as.data.frame() %>%
      setNames("contribution") %>%
      tibble::rownames_to_column("variable") %>%
      dplyr::arrange(dplyr::desc(contribution))
  })

  # Render LDA plot
  output$lda.plot <- renderPlot({
    # validate(need(inherits(rvals$LDAmod,"candisc"),""))
    req(rvals$LDAmod)
    message("rendering LDA plot")
    quietly(label = 'LDA plot',{
      plotLDAvectors(rvals$LDAmod)
    })
  })
}
