


#' UI elements for visualization and group reassignment
#'
#' @return UI
#' @export
#'
#' @examples
#' visualizeassignTab()
visualizeassignTab = function() {
  tabPanel(
    title = "Visualize & Assign",
    id = "visualizetab",
    icon = icon("signal", lib = "glyphicon"),
    tabsetPanel(
      id = "visualize",
      type = "pills",
      tabPanel(
        title = "visualize and select",
        fluidRow(
          column(
            9,
            plotly::plotlyOutput('plot', width = '100%', height = '600px')
          )
        ),
        fluidRow(
          column(
            2,
            selectInput(
              'data.src',
              'Choose data type',
              # choices = c('elements', 'principal components'),
              choices = c('elements', 'principal components','UMAP','linear discriminants'),
              selected = 'elements'
            ),
            uiOutput('xvarUI'),
            uiOutput('yvarUI'),
          ),
          column(
            3,
            offset = 0.5,
            checkboxInput('Conf', 'Data Ellipse',
                          value = TRUE),
            checkboxInput(
              "use_symbols",
              label = bslib::popover(
                tagList(
                  "Use group symbols",
                  trigger = bsicons::bs_icon("info-circle", title = "Help")
                ),
                title = "Group symbols",
                "Applies marker symbols by group. Symbols repeat automatically when groups exceed the available symbol set."
              ),
              value = TRUE
            ),
            checkboxInput(
              "show_point_labels",
              label = bslib::popover(
                tagList(
                  "Show point labels",
                  trigger = bsicons::bs_icon("info-circle", title = "Help")
                ),
                title = "Point labels",
                "Shows a text label next to each point using the selected label column."
              ),
              value = FALSE
            ),
            uiOutput("pointLabelColumnUI"),
            sliderInput(
              'int.set',
              label = bslib::popover(
                tagList("Choose ellipse level",
                        trigger = bsicons::bs_icon("info-circle", title = "Help")
                ),
                title = "Choose ellipse level",
                "Choose the value for the ellipse level. Note that these are data ellipses and not confidence ellipses. For example, if you set level = 0.95, the ellipse will be drawn to represent the region containing approximately 95% of the data points."
              ),
              min = 0.50,
              max = 0.99,
              step = 0.05,
              value = 0.90
            )
          ),
          column(
            3,
            offset = 0.5,
            br(),
            actionButton('Change', 'Change Group Assignment'),
            uiOutput("groupAssignChoiceUI")
          ),
          column(3,
                 offset = 0.5,
                 selectInput('plot_theme', 'Choose plot theme', choices = c('viridis', 'default'), selected = 'viridis'))
        ),
        uiOutput('brush')
      ),
      tabPanel(
        title = "multiplots",
        fluidRow(
          column(3, uiOutput('xvar2UI')),
          column(3,offset = 1,
                 uiOutput('yvar2UI')),
        ),
        fluidRow(
          column(2,
                 radioButtons(
                   inputId = "interactive",
                   label = "make plots interactive?",
                   choices = c(TRUE,FALSE),
                   selected = FALSE),
                 actionButton("updateMultiplot", "update")
          ),
          column(
            2,offset = 1,
            numericInput(
              "plotHeight",
              label = "plot height in pixels",
              min = 500,
              max = 2000,
              value = 900,
              step = 50
            )
          ),
          column(3,offset = 1,sliderInput("ptsize", "plot point size",min = .1, max = 10, value = 2, step = .1,)),
          column(2, offset = 1,actionButton('savePlot', "Save Plot"))
        ),
        fluidRow(uiOutput('multiplotUI'))
      )
    )
  )
}

#' Visualize Assign Server
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
#' visualizeAssignServer(input,output,session,rvals)

validate_multiplot_axes <- function(x_vars, y_vars) {
  x_vars <- as.character(x_vars)
  y_vars <- as.character(y_vars)
  x_vars <- x_vars[!is.na(x_vars) & nzchar(x_vars)]
  y_vars <- y_vars[!is.na(y_vars) & nzchar(y_vars)]

  if (length(x_vars) == 0) {
    return(list(ok = FALSE, message = "Select at least one X variable for multiplot.", x = x_vars, y = y_vars))
  }
  if (length(y_vars) == 0) {
    return(list(ok = FALSE, message = "Select at least one Y variable for multiplot.", x = x_vars, y = y_vars))
  }
  if (length(intersect(x_vars, y_vars)) > 0) {
    return(list(ok = FALSE, message = "X and Y multiplot selections must be different variables.", x = x_vars, y = y_vars))
  }
  list(ok = TRUE, message = "", x = x_vars, y = y_vars)
}

visualizeAssignServer = function(input, output, session, rvals, credentials, con) {
  selected_plot_keys <- shiny::reactiveVal(character())
  multiplot_loading_active <- shiny::reactiveVal(FALSE)

  pick_selected_value <- function(candidate, choices, fallback = "") {
    if (is.null(candidate) || length(candidate) == 0) return(fallback)
    value <- as.character(candidate[[1]])
    if (is.na(value) || !nzchar(value)) return(fallback)
    if (value %in% choices) value else fallback
  }

  show_multiplot_loading <- function() {
    if (isTRUE(multiplot_loading_active())) return(invisible(NULL))
    multiplot_loading_active(TRUE)
    showModal(modalDialog(
      title = NULL,
      footer = NULL,
      class = "transformation-loading-modal",
      easyClose = FALSE,
      tags$div(
        class = "transformation-loading-wrap",
        tags$div(class = "transformation-loading-spinner"),
        tags$div(class = "transformation-loading-text", "Building multiplot...")
      )
    ))
    invisible(NULL)
  }

  hide_multiplot_loading <- function() {
    if (!isTRUE(multiplot_loading_active())) return(invisible(NULL))
    removeModal()
    multiplot_loading_active(FALSE)
    invisible(NULL)
  }

  build_brush_display_table <- function(brush_df) {
    if (!inherits(brush_df, "data.frame") || !"rowid" %in% names(brush_df)) {
      return(brush_df)
    }

    base_df <- if (inherits(rvals$selectedData, "data.frame") && "rowid" %in% names(rvals$selectedData)) {
      rvals$selectedData
    } else {
      brush_df
    }

    id_col <- ""
    if (!is.null(rvals$sampleID) && nzchar(as.character(rvals$sampleID)) && as.character(rvals$sampleID) %in% names(base_df)) {
      id_col <- as.character(rvals$sampleID)
    } else {
      anid_matches <- names(base_df)[tolower(names(base_df)) == "anid"]
      if (length(anid_matches) > 0) id_col <- as.character(anid_matches[[1]])
    }

    chem_cols <- if (is.null(rvals$chem)) character() else intersect(rvals$chem, names(base_df))
    metadata_cols <- setdiff(names(base_df), c("rowid", id_col, chem_cols))
    ordered_cols <- c("rowid", if (nzchar(id_col)) id_col else character(), metadata_cols, chem_cols)
    ordered_cols <- ordered_cols[ordered_cols %in% names(base_df)]

    selected_rowids <- unique(as.character(brush_df$rowid))
    base_df %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::filter(rowid %in% selected_rowids) %>%
      dplyr::select(tidyselect::any_of(ordered_cols))
  }

  output$groupAssignChoiceUI <- renderUI({
    req(rvals$selectedData)
    req(rvals$attrGroups)
    groups <- available_group_assignments(rvals$selectedData, rvals$attrGroups)
    selected_choice <- tryCatch(as.character(input$groupAssignChoice[[1]]), error = function(e) "")
    build_group_assignment_ui(
      choice_input_id = "groupAssignChoice",
      new_input_id = "groupAssignNew",
      groups = groups,
      selected_choice = selected_choice
    )
  })

  observeEvent(input$data.src, {
    if (!identical(input$data.src, "linear discriminants")) return(NULL)
    req(rvals$selectedData)
    req(rvals$attrGroups)
    if (!(rvals$attrGroups %in% names(rvals$selectedData))) return(NULL)
    group_count <- rvals$selectedData %>%
      dplyr::pull(!!as.name(rvals$attrGroups)) %>%
      as.character() %>%
      unique() %>%
      .[!is.na(.)] %>%
      length()
    if (group_count < 3) {
      mynotification("LDA requires at least three groups to visualize these data.", type = "warning")
    }
  }, ignoreInit = TRUE)

  observeEvent({
    rvals$selectedData
    input$data.src
    rvals$pcadf
    rvals$attrGroups
  },{
    req(nrow(rvals$selectedData) > 0)
    req(input$data.src)
    req(rvals$attrGroups)
    quietly(label = "get plotdf",{
      if (input$data.src == 'principal components') {
        validate(need(nrow(rvals$pcadf) > 0, "No PCA results"))
        rvals$plotdf = tryCatch(rvals$pcadf,error = function(e) {
          mynotification("No PCA results",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = pc_columns_sorted(rvals$pca$x %>% colnames())
        rvals$plotVarChoices = pc_axis_choices_with_variance(rvals$pca, rvals$plotVars)
      } else if (input$data.src == 'linear discriminants') {
        validate(need(nrow(rvals$LDAdf) > 0, "No LDA results"))
        rvals$plotdf = tryCatch(rvals$LDAdf,error = function(e) {
          mynotification("No LDA results",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = rvals$LDAdf %>% colnames() %>% .[which(!. %in% rvals$attrs)]
        rvals$plotVarChoices = stats::setNames(rvals$plotVars, rvals$plotVars)
      } else if (input$data.src == 'UMAP') {
        validate(need(nrow(rvals$umapdf) > 0, "No UMAP results"))
        rvals$plotdf = tryCatch(rvals$umapdf,error = function(e) {
          mynotification("No UMAP results",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = c("V1","V2")
        rvals$plotVarChoices = stats::setNames(rvals$plotVars, rvals$plotVars)
      } else {
        req(nrow(rvals$selectedData) > 0)
        rvals$plotdf = tryCatch(rvals$selectedData,error = function(e) {
          mynotification("No data",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = rvals$chem
        rvals$plotVarChoices = stats::setNames(rvals$plotVars, rvals$plotVars)
      }
      if(is.data.frame(rvals$plotdf) && nrow(rvals$plotdf) > 0 && !("rowid" %in% names(rvals$plotdf))){
        rvals$plotdf = rvals$plotdf %>% tibble::rowid_to_column("rowid")
      }
    })
  })

  output$xvarUI = renderUI({
    req(rvals$plotVars)
    req(rvals$plotVarChoices)
    selected_x <- pick_selected_value(rvals$xvar, rvals$plotVars, fallback = rvals$plotVars[1])
    selectInput('xvar', 'X', rvals$plotVarChoices, selected = selected_x)
  })

  output$yvarUI = renderUI({
    req(rvals$plotVars)
    req(rvals$plotVarChoices)
    selected_y <- pick_selected_value(
      rvals$yvar,
      rvals$plotVars,
      fallback = rvals$plotVars[min(2, length(rvals$plotVars))]
    )
    selectInput('yvar', 'y', rvals$plotVarChoices, selected = selected_y)
  })

  output$pointLabelColumnUI = renderUI({
    req(inherits(rvals$plotdf, "data.frame"))
    choices <- names(rvals$plotdf)
    choices <- choices[!is.na(choices) & nzchar(choices)]
    if (length(choices) == 0) return(NULL)
    anid_matches <- choices[tolower(choices) == "anid"]
    point_label_selection <- pick_selected_value(rvals$pointLabelColumn, choices, fallback = "")
    sample_id_selection <- pick_selected_value(rvals$sampleID, choices, fallback = "")
    default_col <- if (nzchar(point_label_selection)) {
      point_label_selection
    } else if (length(anid_matches) > 0) {
      anid_matches[[1]]
    } else if (nzchar(sample_id_selection)) {
      sample_id_selection
    } else if ("rowid" %in% choices) {
      "rowid"
    } else {
      choices[[1]]
    }
    selectInput(
      "pointLabelColumn",
      label = bslib::popover(
        tagList(
          "Label column",
          trigger = bsicons::bs_icon("info-circle", title = "Help")
        ),
        title = "Label column",
        "Select which column to display when point labels are enabled. Defaults to ANID when present."
      ),
      choices = choices,
      selected = default_col
    )
  })

  output$xvar2UI = renderUI({
    req(rvals$chem)
    selectInput('xvar2', 'X', rvals$chem, multiple = T)
  })

  output$yvar2UI = renderUI({
    req(rvals$chem)
    selectInput(
      'yvar2',
      'Y',
      choices = rvals$chem,
      multiple = T,
      selected = rvals$chem
    )
  })

  observeEvent(plotly::event_data("plotly_selected", source = "A"), {
    req(rvals$plotdf)
    plotly_select <- plotly::event_data("plotly_selected", source = "A")
    if (is.null(plotly_select) || !("key" %in% names(plotly_select))) {
      selected_plot_keys(character())
      rvals$brushSelected <- NULL
      return(invisible(NULL))
    }
    keys <- unique(as.character(plotly_select$key))
    selected_plot_keys(keys)
    rvals$brushSelected <- rvals$plotdf %>%
      dplyr::filter(as.character(rowid) %in% keys)
  }, ignoreNULL = FALSE)

  observeEvent(input$Change, {
    req(rvals$plotdf)
    req(rvals$selectedData)
    selected_rows <- rvals$brushSelected
    if (is.null(selected_rows) || nrow(selected_rows) == 0) {
      mynotification("Select one or more points in the plot before changing group assignment.", type = "warning")
      return(invisible(NULL))
    }
    quietly(label = "change group assignment",{
      target_group <- resolve_group_assignment_target(input$groupAssignChoice, input$groupAssignNew)
      if (!nzchar(target_group)) {
        mynotification("Choose an existing group or enter a new group designation.", type = "warning")
        return(invisible(NULL))
      }
      rowid = as.character(selected_rows$rowid)
      replaceCell(rowid = rowid,col = rvals$attrGroups,value = target_group, rvals = rvals, con = con, credentials = credentials, input = input, output = output, session = session)
      selected_keys <- selected_plot_keys()
      if (length(selected_keys) > 0 && inherits(rvals$selectedData, "data.frame") && "rowid" %in% names(rvals$selectedData)) {
        rvals$brushSelected <- rvals$selectedData %>%
          dplyr::filter(as.character(rowid) %in% selected_keys)
      }
      mynotification(glue::glue("Updated {length(rowid)} row(s) to group '{target_group}'."), type = "message")
    })
    inputList = c("xvar","yvar","xvar2","yvar2","data.src","Conf","int.set","use_symbols","show_point_labels","pointLabelColumn")
    for(i in inputList){
      rvals[[i]] = tryCatch(input[[i]],error = function(e)return(NULL))
    }
  })

  # plot
  output$plot <- plotly::renderPlotly({
    validate(need(inherits(rvals$plotdf,"data.frame"),"Waiting for plot data"))
    req(nrow(rvals$plotdf) > 0)
    req(input$xvar %in% names(rvals$plotdf))
    req(input$yvar %in% names(rvals$plotdf))
    req(rvals$attrGroups)
    p <- tryCatch(
      mainPlot(
        plotdf = rvals$plotdf,
        xvar = input$xvar,
        yvar = input$yvar,
        attrGroups = rvals$attrGroups,
        Conf = input$Conf,
        int.set = input$int.set,
        theme = input$plot_theme,
        use_symbols = isTRUE(input$use_symbols),
        show_point_labels = isTRUE(input$show_point_labels),
        label_col = input$pointLabelColumn
      ),
      error = function(e) {
        msg <- paste0("Unable to render plot: ", conditionMessage(e))
        mynotification(msg, type = "error")
        validate(need(FALSE, msg))
      }
    )
    req(p)
    p
  })

  observeEvent(rvals$brushSelected,{
  output$brush <- renderUI({
    req(rvals$brushSelected)
    quietly(label = "brush UI",{
      if (is.null(rvals$brushSelected)) {
        p("Click and drag events (i.e., select/lasso) appear here (double-click to clear)")
      } else {
        tags$div(
          style = "overflow-x:auto; width:100%;",
          tableOutput("brushTable")
        )
      }
    })
  })
  })

  output$brushTable <- renderTable({
    req(rvals$brushSelected)
    build_brush_display_table(rvals$brushSelected)
  }, rownames = FALSE)

  #### multiplots ####

  observeEvent(input$updateMultiplot, {
    show_multiplot_loading()
    ok <- tryCatch({
      if (!inherits(rvals$selectedData, "data.frame") || nrow(rvals$selectedData) == 0) {
        mynotification("No data available for multiplot.", type = "warning")
        return(FALSE)
      }
      axis_check <- validate_multiplot_axes(input$xvar2, input$yvar2)
      if (!isTRUE(axis_check$ok)) {
        mynotification(axis_check$message, type = "warning")
        return(FALSE)
      }

      quietly(label = "multiplot",{
        rvals$multiplot = multiplot(selectedData = rvals$selectedData,attrGroups = rvals$attrGroups,xvar  = axis_check$x, yvar = axis_check$y,ptsize = input$ptsize, interactive = input$interactive, theme = input$plot_theme)
      })
      TRUE
    }, error = function(e) {
      mynotification(paste0("Unable to build multiplot: ", conditionMessage(e)), type = "error")
      rvals$multiplot <- NULL
      FALSE
    }, finally = {
      hide_multiplot_loading()
    })
    if (!isTRUE(ok)) return(invisible(NULL))

    output$multiplotUI = renderUI({
      req(rvals$multiplot)
      if (isTRUE(input$interactive)) {
        plotly::plotlyOutput("multiplotPlotly", width = "100%", height = paste0(input$plotHeight, "px"))
      } else {
        plotOutput("multiplotStatic", width = "100%", height = paste0(input$plotHeight, "px"))
      }
    })

    output$multiplotPlotly = plotly::renderPlotly({
      req(rvals$multiplot)
      req(isTRUE(input$interactive))
      req(inherits(rvals$multiplot, "plotly"))
      rvals$multiplot
    })

    output$multiplotStatic = renderPlot({
      req(rvals$multiplot)
      req(!isTRUE(input$interactive))
      req(inherits(rvals$multiplot, "ggplot"))
      rvals$multiplot
    }, width = "auto", height = function() input$plotHeight)


  })

  observeEvent(input$savePlot, {
    showModal(
      modalDialog(
        title = "Save Plot",
        textInput("plotfilename", "File name:", value = "ggplot.png"),
        numericInput("width", "Width (inches):", value = 7),
        numericInput("height", "Height (inches):", value = 5),
        numericInput("res", "Resolution (dpi):", value = 300),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("saveMultiPlot", "Save")
        )
      )
    )
  }, ignoreInit = TRUE)

  output$saveMultiPlot <- downloadHandler(
    filename = function() {
      input$plotfilename
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = rvals$multiplot,
        width = input$width,
        height = input$height,
        dpi = input$res
      )
    }
  )

}
