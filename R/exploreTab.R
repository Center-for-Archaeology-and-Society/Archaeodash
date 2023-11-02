

#' UI elements for Explore Tab
#'
#' @return
#' @export
#'
#' @examples
exploreTab = function() {
  tabPanel(
    title = "Explore",
    icon = icon("transfer", lib = "glyphicon"),
    id = "explore",
    tabsetPanel(
      type = "pills",
      id = "dataset.impute",
      tabPanel("Elements", fluidRow(
        column(
          3,
          h4(
            "Numbers of samples with missing data by element (pre-imputation)"
          ),
          plotOutput("miss.plot", width = "250px")
        ), column(9, DT::dataTableOutput("elementsDT"))
      )),
      tabPanel("Attributes", DT::dataTableOutput("attributesDT")),
      tabPanel(
        "Crosstabs",
        wellPanel(uiOutput("crosstabsUI")),
        DT::dataTableOutput("crosstabsDT")
      ),
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
  ) # end tabPanel "Impute"
}


#' Explore Server
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
exploreServer = function(input, output, session, rvals) {
  # Render datatable of imputed chemical data
  output$elementsDT <- DT::renderDataTable({
    req(rvals$selectedData)
    quietly(DT::datatable(rvals$selectedData[, rvals$chem], rownames = F))
  })

  # Render datatable of imputed chemical data
  output$attributesDT <- DT::renderDataTable({
    req(rvals$selectedData)
    quietly(DT::datatable(rvals$selectedData[, rvals$attrs], rownames = F))
  })

  output$crosstabsUI = renderUI({
    req(rvals$selectedData)
    fluidRow(
      column(3,
             selectInput(
               inputId = 'crosstab1', "column 1", names(rvals$selectedData)
             )),
      column(
        3,
        offset = 1,
        selectInput(inputId = 'crosstab2', "column 2", names(rvals$selectedData))
      ),
      column(
        3,
        offset = 1,
        selectInput(
          inputId = 'crosstab3',
          "summary function",
          c(
            count = "count",
            mean = "mean",
            median = "median",
            sd = "sd"
          )
        )
      )
    )
  })

  output$crosstabsDT = DT::renderDT({
    req(input$crosstab1)
    quietly({
      if (input$crosstab3 == "count") {
        dt = rvals$selectedData %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(
            c(input$crosstab1, input$crosstab2)
          ))) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop")
      } else {
        suppressWarnings({
          dt = rvals$selectedData %>%
            dplyr::group_by(dplyr::across(tidyselect::all_of(input$crosstab1))) %>%
            dplyr::summarize(dplyr::across(
              .names = paste0("result-", input$crosstab2),
              .cols = tidyselect::all_of(input$crosstab2),
              .fns = list(!!rlang::sym(input$crosstab3))
            ))
        })
      }
      dt
    })
  })


  # Render datatable of transformed chemical data
  output$transform.contents <- DT::renderDataTable({
    req(rvals$selectedData)
    req(rvals$chem)
    quietly(DT::datatable(rvals$selectedData[, rvals$chem], rownames = F))
  })

  # Render missing data plot
  output$miss.plot <- renderPlot({
    validate(need(isTRUE(inherits(
      rvals[['selectedData']], "data.frame"
    )), ""))
    req(rvals[['selectedData']])
    req(rvals$chem)
    plot_missing(data = rvals$selectedData[, rvals$chem])
  })

  # Render UI for univariate displays
  output$ui.univariate <- renderUI({
    req(rvals$selectedData)
    req(rvals$chem)
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
    req(rvals$chem)
    quietly({
      comp.profile(rvals$selectedData[, rvals$chem])
    })
  })

  # Render Element Histogram plot UI
  output$element.hist <- renderPlot({
    req(rvals$chem)
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