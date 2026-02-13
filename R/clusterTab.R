
#' UI elements for cluster tab
#'
#' @return UI
#' @export
#'
#' @examples
#' clusterTab()
clusterTab = function(){
  tabPanel(title = "Cluster", id = "clustertab", icon = icon("adjust", lib = "glyphicon"),
           sidebarLayout(
             sidebarPanel(
               radioButtons("cluster.parent", "Select Clustering Method",
                            choices = c("View optimal number of clusters" = "nClust",
                                        "Hierarchical Agglomerative Clustering" = "hca",
                                        "Hierarchical Divisive Clustering" = "hdca",
                                        "k-means" = "kmeans",
                                        "k-medoids" = "kmedoids"),
                            selected = "nClust"),
               selectInput(
                 "cluster.dataset",
                 "Select dataset to use",
                 choices = c("elements", "principal components", "UMAP", "linear discriminants"),
                 selected = "elements"
               ),
               uiOutput("cluster.options"),
               uiOutput("cluster.column.text"),
               uiOutput("cluster.buttonUI"),
               br(),
               uiOutput("cluster.assign.buttonUI")
             ), # end sidebarPanel

             mainPanel(
               plotOutput("clusterPlot"),
               DT::dataTableOutput("clusterDT")
             ) # end mainPanel PCA
           ) # end sidebarLayout PCA
  ) # end cluster panel
}

#' Cluster Server
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
#' clusterServer(input,output,session,rvals)
clusterServer = function(input,output,session,rvals, credentials, con){
  pending_cluster_assignment <- shiny::reactiveVal(NULL)

  get_cluster_source_features <- function(df, source) {
    if (!is.data.frame(df) || nrow(df) == 0) return(character())
    if (identical(source, "principal components")) {
      cols <- grep("^PC[0-9]+$", names(df), value = TRUE)
    } else if (identical(source, "UMAP")) {
      cols <- grep("^V[0-9]+$", names(df), value = TRUE)
    } else if (identical(source, "linear discriminants")) {
      cols <- grep("^LD[0-9]+$", names(df), value = TRUE)
    } else {
      cols <- intersect(rvals$chem, names(df))
    }
    if (length(cols) == 0) {
      meta_cols <- unique(c(rvals$attrs, rvals$attrGroups, "rowid"))
      candidate_cols <- setdiff(names(df), meta_cols)
      numeric_cols <- candidate_cols[vapply(df[candidate_cols], is.numeric, logical(1))]
      cols <- numeric_cols
    }
    cols
  }

  get_cluster_data <- function(source) {
    if (identical(source, "principal components")) {
      if (!is.data.frame(rvals$pcadf) || nrow(rvals$pcadf) == 0) {
        mynotification("No PCA results available. Run confirm selections with PCA enabled.", type = "warning")
        return(NULL)
      }
      df <- rvals$pcadf
    } else if (identical(source, "UMAP")) {
      if (!is.data.frame(rvals$umapdf) || nrow(rvals$umapdf) == 0) {
        mynotification("No UMAP results available. Run confirm selections with UMAP enabled.", type = "warning")
        return(NULL)
      }
      df <- rvals$umapdf
    } else if (identical(source, "linear discriminants")) {
      if (!is.data.frame(rvals$LDAdf) || nrow(rvals$LDAdf) == 0) {
        mynotification("No LDA results available. Run confirm selections with LDA enabled.", type = "warning")
        return(NULL)
      }
      df <- rvals$LDAdf
    } else {
      df <- rvals$selectedData
    }
    if (!"rowid" %in% names(df)) {
      df <- tibble::rowid_to_column(df, var = "rowid")
    }
    feature_cols <- get_cluster_source_features(df, source)
    if (length(feature_cols) == 0) {
      mynotification("No numeric clustering columns found for this dataset source.", type = "error")
      return(NULL)
    }
    numeric_df <- suppressWarnings(df %>% dplyr::mutate_at(dplyr::vars(feature_cols), as.numeric))
    list(df = numeric_df, features = feature_cols)
  }

  apply_cluster_assignment <- function(assignment_columns, assignment_data) {
    req("rowid" %in% names(assignment_data))
    join_data <- assignment_data %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::distinct(rowid, .keep_all = TRUE)

    rvals$selectedData =
      rvals$selectedData %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::select(-tidyselect::any_of(assignment_columns)) %>%
      dplyr::left_join(join_data, by = "rowid")
    rvals$importedData =
      rvals$importedData %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::select(-tidyselect::any_of(assignment_columns)) %>%
      dplyr::left_join(join_data, by = "rowid")

    if(length(assignment_columns) > 0){
      rvals$attrGroups = assignment_columns[[1]]
      rvals$attrGroupsSub = levels(rvals$selectedData[[rvals$attrGroups]])
      rvals$attrs = unique(c(rvals$attr, rvals$attrGroups, "imputation", "transformation"))
    }

    updateCurrent(rvals,con,credentials,input,output,session)
    showNotification("assigned cluster")
  }

  # Render button to run clustering algorithm
  output$cluster.buttonUI <- renderUI({
    req(rvals$selectedData)
    actionButton("cluster.button", "Run")
  })

  # Render button to run clustering algorithm
  output$cluster.assign.buttonUI <- renderUI({
    req(rvals$selectedData)
    req(input$cluster.parent != "nClust")
    actionButton("cluster.assign.button", "Record cluster assignments")
  })

  # Text input for name of cluster solution assignment column name
  output$cluster.column.text <- renderUI({
    req(rvals$selectedData)
    req(input$cluster.parent != "nClust")
    textInput("cluster.column.text",
              "Input column name for cluster solution")
  })

  # Render WSS and Silhouette graphs for optimal number of clusters for each method

  observeEvent(input$cluster.button, {
    req(nrow(rvals$selectedData) > 0)
    try({
      if(isTruthy(input$cluster.column.text == "")) clusterName = "cluster" else clusterName = input$cluster.column.text
      data_source <- get_cluster_data(input$cluster.dataset)
      if (is.null(data_source)) return(invisible(NULL))
      analysis_df <- data_source$df
      feature_cols <- data_source$features
      feature_data <- analysis_df %>% dplyr::select(tidyselect::any_of(feature_cols))
      rownames(feature_data) <- as.character(analysis_df$rowid)
      if (ncol(feature_data) < 2) {
        mynotification("Clustering requires at least two numeric analysis columns.", type = "error")
        return(invisible(NULL))
      }

      required_pkgs = switch(
        input$cluster.parent,
        nClust = c("factoextra", "cowplot"),
        hca = c("dendextend"),
        hdca = c("dendextend"),
        kmeans = c("factoextra"),
        kmedoids = c("factoextra"),
        character(0)
      )
      if (length(required_pkgs) > 0 && !app_require_packages(required_pkgs, feature = "Cluster analysis")) {
        return(NULL)
      }
      if (input$cluster.parent == "nClust") {
        kmeans_wss <-
          factoextra::fviz_nbclust(feature_data, kmeans, method = "wss") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmeans Elbow Method")
        kmeans_sil <-
          factoextra::fviz_nbclust(feature_data, kmeans, method = "silhouette") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmeans Silhouette Method")
        kmedoids_wss <-
          factoextra::fviz_nbclust(feature_data, cluster::pam, method = "wss") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmedoids Elbow Method")
        kmedoids_sil <-
          factoextra::fviz_nbclust(feature_data, cluster::pam, method = "silhouette") +
          ggplot2::labs(title = "Optimal # of Cluster, Kmedoids ")

        rvals$clusterPlot = function(){cowplot::plot_grid(kmeans_wss, kmeans_sil, kmedoids_wss, kmedoids_sil)}
      } else if (input$cluster.parent == "hca") {
        m <- as.matrix(feature_data)
        rownames(m) <- as.character(analysis_df$rowid)
        hc = as.dendrogram(
          hclust(
            dist(m, method = input$clust.dist.method),
            method = input$hclust.method
          )
        )
        rvals$clusterPlot = function(){plot(
          dendextend::color_branches(hc,
                                     k = input$hca.cutree.k),
          cex.axis = 0.75,
          cex.lab = 0.75,
          horiz = TRUE,
          nodePar = list(
            lab.cex = input$hca.leaf.text.size,
            pch = NA
          ),
          xlab = paste0(
            input$clust.dist.method,
            " distance;",
            input$hclust.method,
            " linkage"
          )
        )}
        rvals$clusterDT <-
          tibble::as_tibble(dendextend::cutree(hc,
                                               k = input$hca.cutree.k))
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "hdca") {
        m <- as.matrix(feature_data)
        rownames(m) <- as.character(analysis_df$rowid)
        hc = as.dendrogram(
          cluster::diana(m, metric = input$hdca.dist.method)
        )
        rvals$clusterPlot = function(){plot(
          dendextend::color_branches(hc,
                                     k = input$hdca.cutree.k),
          cex.axis = 0.75,
          cex.lab = 0.75,
          horiz = TRUE,
          nodePar = list(
            lab.cex = input$hdca.leaf.text.size,
            pch = NA
          ),
          xlab = paste0(input$clust.dist.method, " distance")
        )}
        rvals$clusterDT <-
          tibble::as_tibble(dendextend::cutree(hc,
                                               k = input$hdca.cutree.k))
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample")
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "kmeans") {
        kmeans_solution = kmeans(
          feature_data,
          centers = input$kmeans.centers,
          iter.max = input$kmeans.iter.max,
          nstart = input$kmeans.nstart
        )
        rvals$clusterPlot = function(){factoextra::fviz_cluster(
          kmeans_solution, data = feature_data
        ) +
            ggplot2::theme_bw()}
        rvals$clusterDT = kmeans_solution$cluster
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample") %>%
          dplyr::mutate(Sample = as.character(Sample))
        if (all(rvals$clusterDT$Sample %in% as.character(seq_len(nrow(analysis_df))))) {
          rvals$clusterDT$Sample <- as.character(analysis_df$rowid[as.integer(rvals$clusterDT$Sample)])
        }
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      } else if (input$cluster.parent == "kmedoids") {
        pam_solution =
          cluster::pam(
            feature_data,
            k = input$kmedoids.k,
            metric = input$kmedoids.dist.method
          )
        rvals$clusterPlot = function(){factoextra::fviz_cluster(pam_solution, data = feature_data) + ggplot2::theme_bw()}
        rvals$clusterDT <- pam_solution$cluster
        rvals$clusterDT <-
          tibble::rownames_to_column(as.data.frame(rvals$clusterDT), var = "Sample") %>%
          dplyr::mutate(Sample = as.character(Sample))
        if (all(rvals$clusterDT$Sample %in% as.character(seq_len(nrow(analysis_df))))) {
          rvals$clusterDT$Sample <- as.character(analysis_df$rowid[as.integer(rvals$clusterDT$Sample)])
        }
        colnames(rvals$clusterDT) <-
          c("Sample", clusterName)
      }
    })
  })

  output$clusterPlot <- renderPlot({
    req(rvals$clusterPlot)
    rvals$clusterPlot()
  })

  output$clusterDT<- DT::renderDataTable({
    req(rvals$clusterDT)
    DT::datatable(rvals$clusterDT, rownames = F)
  })


  # Render UI options for cluster analysis
  output$cluster.options <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    # Output of options if HCA chosen
    if (input$cluster.parent == "hca") {
      cluster_input_selections <- list(
        # HCA distance method
        selectInput(
          "clust.dist.method",
          label = "Select HCA Distance Method",
          choices = list(
            "Euclidean" = "euclidean",
            "Manhattan" = "manhattan",
            "Minkowski" = "minkowski",
            "Maximum" = "maximum"
          ),
          selected = "euclidean"
        ),
        # HCA linkage criterion choices
        selectInput(
          "hclust.method",
          label = ("Select HCA Linkage Criterion"),
          choices = list(
            "Average Linkage" = "average",
            "Complete Linkage" = "complete",
            "Ward's" = "ward.D",
            "Ward's squared" = "ward.D2"
          ),
          selected = "average"
        ),
        # HCA dendrogram leaf text size
        numericInput(
          "hca.leaf.text.size",
          label = "Leaf Text Size",
          value = 1,
          min = 0.05,
          max = 10,
          step = 0.05
        ),
        # HCA dendrogram cutree clusters
        numericInput(
          "hca.cutree.k",
          label = "Choose Numer of Clusters",
          value = 1,
          min = 1,
          max = 500,
          step = 1
        )
      )

    } else if (input$cluster.parent == "hdca") {
      # Output of options if HDCA is chosen
      cluster_input_selections <- list(
        # HDCA distance method
        selectInput(
          "hdca.dist.method",
          label = "Select HDCA Distance Method",
          choices = list("Euclidean" = "euclidean",
                         "Manhattan" = "manhattan"),
          selected = "euclidean"
        ),
        # HCDA dendrogram leaf text size
        numericInput(
          "hdca.leaf.text.size",
          label = "Leaf Text Size",
          value = 1,
          min = 0.05,
          max = 10,
          step = 0.05
        ),
        # HCDA dendrogram cutree clusters
        numericInput(
          "hdca.cutree.k",
          label = "Choose Numer of Clusters",
          value = 1,
          min = 1,
          max = 500,
          step = 1
        )
      )

    } else if (input$cluster.parent == "kmeans") {
      # Output of options if k-means is chosen
      cluster_input_selections <- list(
        # k-means number of centers
        numericInput(
          "kmeans.centers",
          label = "Choose Number of Clusters",
          value = 2,
          min = 1,
          max = 20,
          step = 1
        ),
        # k-means number of random initial configurations
        # best one is chosen and used
        numericInput(
          "kmeans.nstart",
          label = "Choose Number of Initial Configurations",
          value = 5,
          min = 1,
          max = 100,
          step = 1
        ),
        # k-means number of maximum iterations to converge and
        # reach stopping criterion
        numericInput(
          "kmeans.iter.max",
          label = "Maximum Number of Iterations",
          value = 10,
          min = 1,
          max = 200,
          step = 1
        )
      )
    } else if (input$cluster.parent == "kmedoids") {
      # Output of options if k-medoids is chosen
      cluster_input_selections <- list(
        # k-medoids Distance Method choices
        selectInput(
          "kmedoids.dist.method",
          label = "Select HDCA Distance Method",
          choices = list("Euclidean" = "euclidean",
                         "Manhattan" = "manhattan"),
          selected = "euclidean"
        ),
        # k-medoids number of clusters
        numericInput(
          "kmedoids.k",
          label = "Choose Number of Clusters",
          value = 2,
          min = 1,
          max = 20,
          step = 1
        )
      )
    } else {
      cluster_input_selections = NULL
    }

    # Initialize selections based on clustering method chosen
    cluster_input_selections
  })

  # Assign cluster assignments based on cluster solution

  observeEvent(input$cluster.assign.button,{
    req(rvals$clusterDT)
    quietly({
      assignment_columns = rvals$clusterDT %>% names()
      assignment_columns = setdiff(assignment_columns,'Sample')
      assignment_data = rvals$clusterDT %>%
        dplyr::rename(rowid = Sample) %>%
        dplyr::select(rowid, tidyselect::any_of(assignment_columns)) %>%
        dplyr::mutate_at(dplyr::vars(assignment_columns),factor)

      existing_columns = intersect(assignment_columns, names(rvals$selectedData))
      if(length(existing_columns) > 0){
        pending_cluster_assignment(list(
          assignment_columns = assignment_columns,
          assignment_data = assignment_data
        ))
        showModal(modalDialog(
          title = "Overwrite existing column?",
          p(paste0(
            "The following column(s) already exist and will be overwritten: ",
            paste(existing_columns, collapse = ", "),
            "."
          )),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirmClusterOverwrite", "Overwrite")
          ),
          easyClose = TRUE
        ))
      } else {
        apply_cluster_assignment(assignment_columns, assignment_data)
      }
    })
  })

  observeEvent(input$confirmClusterOverwrite, {
    req(pending_cluster_assignment())
    pending = pending_cluster_assignment()
    removeModal()
    apply_cluster_assignment(
      assignment_columns = pending$assignment_columns,
      assignment_data = pending$assignment_data
    )
    pending_cluster_assignment(NULL)
  })

}
