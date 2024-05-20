
#' UI elements for ordination
#'
#' @return UI
#' @export
#'
#' @examples
#' ordinationTab()
ordinationTab = function(){
  tabPanel(title = "Ordination", id = "ordinationtab",icon = icon("equalizer", lib = "glyphicon"),
             tabsetPanel(id = "ordination", type = "pills",
                         tabPanel("PCA",
                                  fluidRow(column(6,
                                                  uiOutput('pcaheader'))),
                                  fluidRow(
                                    column(6, plotly::plotlyOutput("pca.plot")),
                                    column(6, plotly::plotlyOutput("pca.el.plot"))),
                                  fluidRow(
                                    column(6, plotly::plotlyOutput("eigen.plot")),
                                    column(6,tableOutput("contribTbl")))
                         ), # end tabPanel PCA
                         tabPanel("UMAP",                                  fluidRow(column(6,
                                                                                           uiOutput('umapheader'))),
                                  fluidRow(column(6,
                                                  plotly::plotlyOutput("umap.plot")))
                                  ),
                         tabPanel("LDA",
                                  fluidRow(column(6,
                                                  uiOutput('ldaheader'))),
                                  fluidRow(column(6,
                                                  plotly::plotlyOutput("lda.plot")))
                         ) # end tabPanel LDA
             ) # end tabsetPanel
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

  output$pcaheader = renderUI({
    # invalidateLater(1000)
    if(isTruthy(rvals$pcadf)){
      h2("Principal Component Analysis")
    } else {
      h2("Please run PCA first")
    }
  })

  output$umapheader = renderUI({
    # invalidateLater(1000)
    if(isTruthy(rvals$umapdf)){
      h2("Uniform Manifold Approximation and Projection (UMAP)")
    } else {
      h2("Please run UMAP first")
    }
  })

  output$ldaheader = renderUI({
    # invalidateLater(1000)
    if(isTruthy(rvals$pcadf)){
      h2("Linear Discriminant Analysis")
    } else {
      h2("Please run LDA first")
    }
  })

  observeEvent(rvals$runPCA, {
    req(rvals$runPCA)
    req(rvals$selectedData)
    message("running PCA")
    quietly(label = "running PCA",{
      if(isTRUE(rvals$runPCA)){
        rvals$pca = prcomp(rvals$selectedData[,rvals$chem])
        rvals$pcadf = dplyr::bind_cols(rvals$selectedData %>% dplyr::select(-tidyselect::any_of(rvals$chem)),rvals$pca$x)
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

  observeEvent(rvals$runUMAP, {
    req(rvals$runUMAP)
    req(rvals$selectedData)
    message("running UMAP")
    quietly(label = 'running UMAP',{
      if(isTRUE(rvals$runUMAP)){
        umap = tryCatch(umap::umap(rvals$selectedData %>% dplyr::select(tidyselect::any_of(rvals$chem))),error = function(e){
          mynotification(paste("UMAP failed", "UMAP failed to run. Please check your data and try again.\n",e))
          return(NULL)
        } )
        rvals$umapdf = dplyr::bind_cols(rvals$selectedData %>% dplyr::select(-tidyselect::any_of(rvals$chem)),umap$layout %>%
          as.data.frame())
        rvals$runUMAP = F
      }
    })
  })

  # Render PCA plot
  output$pca.plot <- plotly::renderPlotly({
    req(rvals$pca)
    message("rendering PCA plot")
    quietly(label = 'PCA plot',{
      plotly::ggplotly(factoextra::fviz_pca_ind(
        rvals$pca,
        col.ind = "cos2",
        # Color by the quality of representation
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        label = 'none',
        repel = F
      ))
    })
  })

  # Render PCA Eigenvalue plot
  output$eigen.plot <- plotly::renderPlotly({
    req(rvals$pca)
    message("rendering eigenvalue plot")
    quietly(label = "PCA plot 2",{
      plotly::ggplotly(factoextra::fviz_eig(rvals$pca))
    })
  })

  # Render PCA Eigenvalue plot
  output$pca.el.plot <- plotly::renderPlotly({
    req(rvals$pca)
    quietly(label = "PCA plot 3",{
      plotly::ggplotly(factoextra::fviz_pca_var(
        rvals$pca,
        col.var = "contrib",
        # Color by contributions to the PC
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        repel = F
      ))
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
  output$lda.plot <- plotly::renderPlotly({
    # validate(need(inherits(rvals$LDAmod,"candisc"),""))
    req(rvals$LDAmod)
    message("rendering LDA plot")
    quietly(label = 'LDA plot',{
      pdf(file = NULL)
      plotly::ggplotly(plotLDAvectors(rvals$LDAmod))
    })
  })

  # Render UMAP plot
  output$umap.plot <- plotly::renderPlotly({
    req(rvals$umapdf)
    message("rendering UMAP plot")
    quietly(label = 'UMAP plot',{
      g = ggplot2::ggplot(rvals$umapdf,ggplot2::aes(x = V1, y = V2, color = !!as.name(rvals$attrGroups))) +
        ggplot2::geom_point() +
        ggplot2::stat_ellipse() +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "UMAP")
      plotly::ggplotly(p = g)
    })
  })
}
