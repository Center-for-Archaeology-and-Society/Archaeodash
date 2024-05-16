

#' UI elements for Explore Tab
#'
#' @return NULL
#' @export
#'
#' @examples
#' exploreTab()
exploreTab = function() {
  tabPanel(
    title = "Explore",
    id = "exploretab",
    icon = icon("transfer", lib = "glyphicon"),
    tabsetPanel(
      type = "pills",
      id = "dataset.impute",
      tabPanel("Dataset", fluidRow(
        column(
          3,
          h4(
            "Numbers of samples with missing data by element (pre-imputation)"
          ),
          plotOutput("miss.plot", width = "250px")
        ), column(9, DT::dataTableOutput("datasetDT"))
      )),
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
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param rvals reactive values object
#'
#' @return server
#' @export
#'
#' @examples
#' exploreServer(input,output,session,rvals)
exploreServer = function(input, output, session, rvals, con, credentials) {
  # Render datatable of imputed chemical data
  output$datasetDT <- DT::renderDataTable({
    req(rvals$selectedData)
    quietly(DT::datatable(rvals$selectedData, rownames = F,editable = TRUE))
  })

  observeEvent(input$datasetDT_cell_edit, {
    info <- input$datasetDT_cell_edit
    str(info)  # Print info to the console for debugging
    i <- info$row
    j <- info$col + 1
    v <- info$value
    rvals$selectedData = rvals$selectedData %>%
      dplyr::mutate_if(is.factor, as.character)
    rvals$importedData = rvals$importedData %>%
      dplyr::mutate_if(is.factor, as.character)
    rvals$selectedData[i, j] <- DT::coerceValue(v, rvals$selectedData[i, j])
    rowid = rvals$selectedData$rowid[i]
    # print(rowid)
    col = names(rvals$selectedData)[j]
    rvals$importedData[rowid, col] <- DT::coerceValue(v, rvals$importedData[rowid, col])
    rvals$selectedData = rvals$selectedData %>%
      dplyr::mutate_if(is.character, as.factor)
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
      suppressWarnings({
      if (input$crosstab3 == "count") {
        dt = rvals$selectedData %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(
            c(input$crosstab1, input$crosstab2)
          ))) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop")
      } else {

          dt = rvals$selectedData %>%
            dplyr::mutate_at(dplyr::vars(tidyselect::all_of(input$crosstab2),as.numeric)) %>%
            dplyr::group_by(dplyr::across(tidyselect::all_of(input$crosstab1))) %>%
            dplyr::summarize(dplyr::across(
              .names = paste0("result-", input$crosstab2),
              .cols = tidyselect::all_of(input$crosstab2),
              .fns = list(!!rlang::sym(input$crosstab3))
            ))
      }
      })
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
      plotdf = rvals$selectedData[, rvals$chem] %>%
        dplyr::mutate_all(quietly(as.numeric))
      ggplot2::ggplot(data = plotdf,
                      ggplot2::aes_string(x = input$hist.el)) +
        ggplot2::geom_histogram(fill = "blue",
                                alpha = 0.5,
                                bins = input$hist.bin) +
        ggplot2::labs(x = input$hist.el, y = " ")
    })
  })
}
