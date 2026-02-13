

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
          h5(
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
        plotly::plotlyOutput("comp.profile")
      )
    )
  ) # end tabPanel "Impute"
}

# Internal helper for Crosstabs calculations.
compute_crosstab_summary <- function(data, group_column, value_column, summary_method) {
  if (!inherits(data, "data.frame")) {
    stop("No data available.", call. = FALSE)
  }

  column_names <- names(data)
  if (!(group_column %in% column_names)) {
    stop("Column 1 is not available.", call. = FALSE)
  }
  if (!(value_column %in% column_names)) {
    stop("Column 2 is not available.", call. = FALSE)
  }

  if (summary_method == "count") {
    return(
      data %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_column, value_column)))) %>%
        dplyr::summarize(count = dplyr::n(), .groups = "drop")
    )
  }

  summary_function <- switch(
    summary_method,
    mean = mean,
    median = median,
    sd = sd,
    NULL
  )
  if (is.null(summary_function)) {
    stop("Unsupported summary function selected.", call. = FALSE)
  }

  summary_input <- data %>%
    dplyr::mutate(
      crosstab_numeric = suppressWarnings(as.numeric(as.character(.data[[value_column]])))
    )

  if (!any(!is.na(summary_input$crosstab_numeric))) {
    stop(paste0("Column '", value_column, "' cannot be converted to numeric values."), call. = FALSE)
  }

  result_column_name <- paste0("result-", value_column)
  summary_input %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group_column))) %>%
    dplyr::summarize(
      !!result_column_name := round(summary_function(crosstab_numeric, na.rm = TRUE), 2),
      .groups = "drop"
    )
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
    updateCurrent(rvals,con,credentials,input,output,session)
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
    req(rvals$selectedData, input$crosstab1, input$crosstab2, input$crosstab3)
    quietly({
      tryCatch(
        compute_crosstab_summary(
          data = rvals$selectedData,
          group_column = input$crosstab1,
          value_column = input$crosstab2,
          summary_method = input$crosstab3
        ),
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )
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
      rvals[['importedData']], "data.frame"
    )), ""))
    req(rvals[['importedData']])
    chem_for_plot = if (!is.null(rvals$initialChem)) rvals$initialChem else rvals$chem
    chem_for_plot = chem_for_plot[chem_for_plot %in% names(rvals$importedData)]
    validate(need(length(chem_for_plot) > 0, "No element columns available for missing-data plot."))
    plot_missing(data = rvals$importedData[, chem_for_plot, drop = FALSE])
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
  output$comp.profile <- plotly::renderPlotly({
    req(rvals$selectedData)
    req(rvals$chem)
    quietly({
      groups = NULL
      if (!is.null(rvals$attrGroups) && rvals$attrGroups %in% names(rvals$selectedData)) {
        groups = rvals$selectedData[[rvals$attrGroups]]
      }
      p = comp.profile(rvals$selectedData[, rvals$chem], groups = groups)
      plotly::ggplotly(p)
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
