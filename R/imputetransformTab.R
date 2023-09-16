


#' UI elements for Impute - Transform Tab
#'
#' @return
#' @export
#'
#' @examples
imputetransformTab = function() {
  tabPanel(
    title = "Impute & Transform",
    icon = icon("transfer", lib = "glyphicon"),
    id = "imputetransform",

    sidebarLayout(
      sidebarPanel(
        uiOutput("impute.options"),
        br(),
        uiOutput("ui.impute"),
        tags$hr(),
        uiOutput("transform.options"),
        br(),
        uiOutput("ui.transform"),
        tags$hr(),
        "Numbers of samples with missing data by element (pre-imputation)",
        plotOutput("miss.plot", width = "250px")
      ),
      # end sidebarPanel

      mainPanel(
        tabsetPanel(
          type = "pills",
          id = "dataset.impute",
          tabPanel("Elements", DT::dataTableOutput("elementsDT")),
          tabPanel("Attributes", DT::dataTableOutput("attributesDT")),
          tabPanel(
            "Univariate Plots",
            uiOutput("ui.univariate"),
            uiOutput("ui.hist.bin"),
            plotOutput("element.hist")
          ),
          tabPanel(
            "Compositional Profile Plot",
            br(),
            uiOutput("ui.comp"),
            plotOutput("comp.profile")
          )
        )
      ) # end mainPanel Impute
    ) # end sidebarLayout Impute
  ) # end tabPanel "Impute"
}


#' Impute and Transform Server
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
imputeTransformServer = function(input, output, session, rvals) {
  # Render options for data imputation
  output$impute.options <- renderUI({
    req(rvals$selectedData)
    radioButtons(
      "impute.method",
      label = ("Select Imputation Method"),
      choices = list(
        "None" = "none",
        "Random Forest" = "rf",
        "Predictive Mean Matching" = "pmm",
        "Weighted Predictive Mean Matching" = "midastouch"
      ),
      selected = "none"
    )
  })

  # Render button and controls to Impute data
  output$ui.impute <- renderUI({
    req(rvals$selectedData)
    actionButton("impute", "Impute missing data")
  })

  observeEvent(input$impute, {
    req(input$impute.method)
    if (input$impute.method != "none") {
      showNotification("imputing data")
      quietly({
        rvals$selectedData[, rvals$chem] = mice::complete(mice::mice(rvals$selectedData[, rvals$chem], method = input$impute.method))
      })
      showNotification("completed imputing data")
    }
  })

  # Render datatable of imputed chemical data
  output$elementsDT <- DT::renderDataTable({
    req(rvals$selectedData)
    quietly(DT::datatable(rvals$selectedData[, rvals$chem], rownames = F))
  })

  # Render datatable of imputed chemical data
  output$attributesDT <- DT::renderDataTable({
    req(rvals$selectedData)
    quietly(DT::datatable(rvals$selectedData[,rvals$attrs], rownames = F))
  })


  # Render button and controls to transform data
  output$ui.transform <- renderUI({
    req(rvals$selectedData)
    actionButton("transform", "Transform data")
  })

  # Render options for data transformation
  output$transform.options <- renderUI({
    req(req(rvals$selectedData))
    radioButtons(
      "transform.method",
      label = ("Select Transformation"),
      choices = list(
        "None" = "none",
        "Log-10" = "log10",
        "Natural Log" = "log",
        "Percent/Z-score" = "zScore"
      ),
      selected = "none"
    )
  })

  observeEvent(input$transform, {
    req(rvals$selectedData)
    print("starting transform")
    quietly({
      suppressWarnings({
        if (input$transform.method == 'zscale') {
          rvals$selectedData[, rvals$chem] = zScale(rvals$selectedData[, rvals$chem])
        } else if (input$transform.method %in% c("log10", "log")) {
          rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
            dplyr::mutate_all(input$transform.method) %>%
            dplyr::mutate_all(round, digits = 3)
        } else if (input$transform.method == "none") {
          rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
            dplyr::mutate_all(round, digits = 3)
        }
      })
      # get rid of infinite values
      rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
        dplyr::mutate_all(list(function(c) dplyr::case_when(!is.finite(c) ~ 0, TRUE ~ c)))
    })
    print("ending transform")
  })

  # Render datatable of transformed chemical data
  output$transform.contents <- DT::renderDataTable({
    req(rvals$selectedData)
    quietly(
      DT::datatable(rvals$selectedData[, rvals$chem], rownames = F)
    )
  })

  # Render missing data plot
  output$miss.plot <- renderPlot({
    req(rvals$selectedData)
    DataExplorer::plot_missing(rvals$selectedData[, rvals$chem], ggtheme = ggplot2::theme_bw())
  })

  # Render UI for univariate displays
  output$ui.univariate <- renderUI({
    req(rvals$selectedData)
    selectInput("hist.el",
                "Element",
                choices = names(rvals$selectedData[, rvals$chem]))
  })

  # Render UI for univariate displays
  output$ui.hist.bin <- renderUI({
    req(rvals$selectedData)
    sliderInput(
      "hist.bin",
      "Number of Bins",
      min = 2,
      max = 100,
      value = 30,
      step = 1
    )
  })

  # # Render reset button for compositional profile plot
  # output$ui.comp <- renderUI({
  #   req(input$file1)
  #   actionButton("comp.reset", "Reset Plot")
  # })

  # Render compositional profile plot
  output$comp.profile <- renderPlot({
    req(rvals$selectedData)
    quietly({
      comp.profile(rvals$selectedData[, rvals$chem])
    })
  })

  # Render Element Histogram plot UI
  output$element.hist <- renderPlot({
    quietly({
      if (length(rvals$selectedData[, rvals$chem][input$hist.el]) == 0)
        return(NULL)
      ggplot2::ggplot(data = rvals$selectedData[, rvals$chem],
                      ggplot2::aes_string(x = input$hist.el)) +
        ggplot2::geom_histogram(fill = "blue",
                                alpha = 0.5,
                                bins = input$hist.bin) +
        ggplot2::labs(x = input$hist.el, y = " ")
    })
  })
}
