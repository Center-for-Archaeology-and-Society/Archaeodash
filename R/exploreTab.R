

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

# Internal helper for Crosstabs calculations.
build_crosstab_summary <- function(data, col1, col2, summary_type) {
  if (!inherits(data, "data.frame")) {
    stop("No data available.", call. = FALSE)
  }

  col_names <- names(data)
  if (!(col1 %in% col_names)) {
    stop("Column 1 is not available.", call. = FALSE)
  }
  if (!(col2 %in% col_names)) {
    stop("Column 2 is not available.", call. = FALSE)
  }

  if (summary_type == "count") {
    return(
      data %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(c(col1, col2)))) %>%
        dplyr::summarize(count = dplyr::n(), .groups = "drop")
    )
  }

  summary_fn <- switch(
    summary_type,
    mean = mean,
    median = median,
    sd = sd,
    NULL
  )
  if (is.null(summary_fn)) {
    stop("Unsupported summary function selected.", call. = FALSE)
  }

  dt_input <- data %>%
    dplyr::mutate(
      crosstab_numeric = suppressWarnings(as.numeric(as.character(.data[[col2]])))
    )

  if (!any(!is.na(dt_input$crosstab_numeric))) {
    stop(paste0("Column '", col2, "' cannot be converted to numeric values."), call. = FALSE)
  }

  result_name <- paste0("result-", col2)
  dt_input %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(col1))) %>%
    dplyr::summarize(
      !!result_name := round(summary_fn(crosstab_numeric, na.rm = TRUE), 2),
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
        build_crosstab_summary(
          data = rvals$selectedData,
          col1 = input$crosstab1,
          col2 = input$crosstab2,
          summary_type = input$crosstab3
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
