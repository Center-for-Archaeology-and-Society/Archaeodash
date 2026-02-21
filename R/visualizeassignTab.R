


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
        uiOutput("visualizeSelectLayout"),
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

resolve_multiplot_y_selection <- function(all_vars, x_vars, y_vars = character()) {
  all_vars <- as.character(all_vars)
  all_vars <- all_vars[!is.na(all_vars) & nzchar(all_vars)]

  x_vars <- as.character(x_vars)
  x_vars <- x_vars[!is.na(x_vars) & nzchar(x_vars)]

  y_vars <- as.character(y_vars)
  y_vars <- y_vars[!is.na(y_vars) & nzchar(y_vars)]

  choices <- setdiff(all_vars, x_vars)
  selected <- intersect(y_vars, choices)
  if (length(selected) == 0) {
    selected <- choices
  }

  list(choices = choices, selected = selected)
}

resolve_filters_below_plot_default <- function(plot_width, threshold = 768) {
  width <- suppressWarnings(as.numeric(plot_width))
  if (length(width) == 0 || is.na(width[[1]]) || !is.finite(width[[1]])) {
    return(NULL)
  }
  width[[1]] <= threshold
}

visualizeAssignServer = function(input, output, session, rvals, credentials, con) {
  selected_plot_keys <- shiny::reactiveVal(character())
  multiplot_loading_active <- shiny::reactiveVal(FALSE)
  multiplot_request_counter <- shiny::reactiveVal(0L)
  multiplot_active_request_id <- shiny::reactiveVal(NA_integer_)
  multiplot_cancelled_request_id <- shiny::reactiveVal(NA_integer_)
  multiplot_mode <- shiny::reactiveVal(FALSE)
  multiplot_height <- shiny::reactiveVal(900)
  multiplot_build_promise <- shiny::reactiveVal(NULL)
  auto_filters_layout_set <- shiny::reactiveVal(FALSE)

  pick_selected_value <- function(candidate, choices, fallback = "") {
    if (is.null(candidate) || length(candidate) == 0) return(fallback)
    value <- as.character(candidate[[1]])
    if (is.na(value) || !nzchar(value)) return(fallback)
    if (value %in% choices) value else fallback
  }

  show_multiplot_loading <- function() {
    if (isTRUE(multiplot_loading_active())) return(invisible(NULL))
    try(removeModal(), silent = TRUE)
    multiplot_loading_active(TRUE)
    showModal(modalDialog(
      title = NULL,
      footer = tagList(
        actionButton("cancelMultiplotBuild", "Cancel")
      ),
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
    try(removeModal(), silent = TRUE)
    multiplot_loading_active(FALSE)
    invisible(NULL)
  }

  interactive_mode <- function(x) {
    isTRUE(suppressWarnings(as.logical(x)))
  }

  async_multiplot_enabled <- requireNamespace("promises", quietly = TRUE) &&
    requireNamespace("future", quietly = TRUE)
  if (isTRUE(async_multiplot_enabled) && !isTRUE(getOption("archaeodash.multiplot.future_plan_initialized"))) {
    future::plan(future::multisession, workers = 1)
    options(archaeodash.multiplot.future_plan_initialized = TRUE)
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

  build_visualize_controls <- function(scroll_controls = TRUE) {
    controls_style <- if (isTRUE(scroll_controls)) {
      "max-height:600px; overflow-y:auto; padding-left:8px;"
    } else {
      "padding-top:8px; padding-left:8px; padding-right:8px;"
    }
    if (isTRUE(scroll_controls)) {
      return(tags$div(
        style = controls_style,
        checkboxInput(
          "filters_below_plot",
          "Place filters below plot",
          value = isTRUE(input$filters_below_plot)
        ),
        selectInput(
          'data.src',
          'Choose data type',
          choices = c('elements', 'principal components','UMAP','linear discriminants'),
          selected = 'elements'
        ),
        uiOutput('xvarUI'),
        uiOutput('yvarUI'),
        selectInput('plot_theme', 'Choose plot theme', choices = c('viridis', 'default'), selected = 'viridis'),
        hr(),
        h5("Visualization filter"),
        uiOutput("vizMetaFilterFieldUI"),
        uiOutput("vizMetaFilterValuesUI"),
        actionButton("clearVizFilter", "Clear visualization filter"),
        hr(),
        checkboxInput('Conf', 'Data Ellipse', value = TRUE),
        checkboxInput(
          "use_symbols",
          label = bslib::popover(
            tagList(
              "Use group symbols",
              trigger = bsicons::bs_icon("info-circle", title = "Help")
            ),
            title = "Group symbols",
            "Applies marker symbols by metadata values. Defaults to the current group column and repeats symbols when values exceed the available symbol set."
          ),
          value = TRUE
        ),
        uiOutput("symbolGroupColumnUI"),
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
        ),
        br(),
        actionButton('Change', 'Change Group Assignment'),
        uiOutput("groupAssignChoiceUI")
      ))
    }

    tags$div(
      style = controls_style,
      fluidRow(
        column(3, checkboxInput("filters_below_plot", "Place filters below plot", value = isTRUE(input$filters_below_plot))),
        column(3, selectInput('data.src', 'Choose data type', choices = c('elements', 'principal components','UMAP','linear discriminants'), selected = 'elements')),
        column(3, uiOutput('xvarUI')),
        column(3, uiOutput('yvarUI'))
      ),
      fluidRow(
        column(3, selectInput('plot_theme', 'Choose plot theme', choices = c('viridis', 'default'), selected = 'viridis')),
        column(3, checkboxInput('Conf', 'Data Ellipse', value = TRUE)),
        column(3, checkboxInput(
          "use_symbols",
          label = bslib::popover(
            tagList(
              "Use group symbols",
              trigger = bsicons::bs_icon("info-circle", title = "Help")
            ),
            title = "Group symbols",
            "Applies marker symbols by metadata values. Defaults to the current group column and repeats symbols when values exceed the available symbol set."
          ),
          value = TRUE
        )),
        column(3, uiOutput("symbolGroupColumnUI"))
      ),
      fluidRow(
        column(3, checkboxInput(
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
        )),
        column(3, uiOutput("pointLabelColumnUI")),
        column(6, sliderInput(
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
        ))
      ),
      hr(),
      h5("Visualization filter"),
      fluidRow(
        column(3, uiOutput("vizMetaFilterFieldUI")),
        column(6, uiOutput("vizMetaFilterValuesUI")),
        column(3, br(), actionButton("clearVizFilter", "Clear visualization filter"))
      ),
      hr(),
      fluidRow(
        column(3, actionButton('Change', 'Change Group Assignment')),
        column(9, uiOutput("groupAssignChoiceUI"))
      )
    )
  }

  output$visualizeSelectLayout <- renderUI({
    if (isTRUE(input$filters_below_plot)) {
      tagList(
        fluidRow(
          column(12, plotly::plotlyOutput('plot', width = '100%', height = '600px'))
        ),
        fluidRow(
          column(12, build_visualize_controls(scroll_controls = FALSE))
        )
      )
    } else {
      fluidRow(
        column(9, plotly::plotlyOutput('plot', width = '100%', height = '600px')),
        column(3, build_visualize_controls(scroll_controls = TRUE))
      )
    }
  })

  observe({
    if (isTRUE(auto_filters_layout_set())) return(invisible(NULL))
    default_layout <- resolve_filters_below_plot_default(session$clientData$output_plot_width, threshold = 768)
    if (is.null(default_layout)) return(invisible(NULL))
    shiny::updateCheckboxInput(session, "filters_below_plot", value = isTRUE(default_layout))
    auto_filters_layout_set(TRUE)
    invisible(NULL)
  })

  missing_filter_label <- "(Missing)"
  normalize_filter_values <- function(x) {
    vals <- as.character(x)
    vals[is.na(vals) | !nzchar(trimws(vals))] <- missing_filter_label
    vals
  }

  metadata_filter_fields <- shiny::reactive({
    if (!inherits(rvals$selectedData, "data.frame") || nrow(rvals$selectedData) == 0) return(character())
    chem_cols <- if (is.null(rvals$chem)) character() else intersect(rvals$chem, names(rvals$selectedData))
    fields <- setdiff(names(rvals$selectedData), c("rowid", chem_cols))
    fields[!is.na(fields) & nzchar(fields)]
  })

  plot_df_for_display <- shiny::reactive({
    req(inherits(rvals$plotdf, "data.frame"))
    plot_df <- rvals$plotdf
    if (!("rowid" %in% names(plot_df)) || nrow(plot_df) == 0) return(plot_df)
    field <- tryCatch(as.character(input$vizMetaFilterField[[1]]), error = function(e) "")
    selected_vals <- tryCatch(as.character(input$vizMetaFilterValues), error = function(e) character())
    selected_vals <- selected_vals[!is.na(selected_vals) & nzchar(selected_vals)]

    if (!nzchar(field) || length(selected_vals) == 0) return(plot_df)
    if (!inherits(rvals$selectedData, "data.frame")) return(plot_df)
    if (!(field %in% names(rvals$selectedData)) || !("rowid" %in% names(rvals$selectedData))) return(plot_df)

    matched_rowids <- rvals$selectedData %>%
      dplyr::mutate(
        rowid = as.character(rowid),
        .filter_value = normalize_filter_values(.data[[field]])
      ) %>%
      dplyr::filter(.data$.filter_value %in% selected_vals) %>%
      dplyr::pull(rowid) %>%
      unique()

    plot_df %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::filter(.data$rowid %in% matched_rowids)
  })

  output$vizMetaFilterFieldUI <- renderUI({
    fields <- metadata_filter_fields()
    if (length(fields) == 0) return(NULL)
    current <- tryCatch(as.character(input$vizMetaFilterField[[1]]), error = function(e) "")
    if (length(current) == 0 || is.na(current[[1]]) || !nzchar(current[[1]])) {
      current <- ""
    } else {
      current <- current[[1]]
    }
    selected_field <- if (current %in% fields) current else fields[[1]]
    selectInput(
      "vizMetaFilterField",
      "Metadata field",
      choices = fields,
      selected = selected_field
    )
  })

  output$vizMetaFilterValuesUI <- renderUI({
    req(inherits(rvals$selectedData, "data.frame"))
    field <- tryCatch(as.character(input$vizMetaFilterField[[1]]), error = function(e) "")
    if (length(field) == 0 || is.na(field[[1]]) || !nzchar(field[[1]])) return(NULL)
    field <- field[[1]]
    req(field %in% names(rvals$selectedData))
    choices <- rvals$selectedData %>%
      dplyr::pull(!!as.name(field)) %>%
      normalize_filter_values() %>%
      unique() %>%
      sort()
    selected_vals <- tryCatch(as.character(input$vizMetaFilterValues), error = function(e) character())
    selected_vals <- intersect(selected_vals, choices)
    selectizeInput(
      "vizMetaFilterValues",
      "Metadata values (multi-select)",
      choices = choices,
      selected = selected_vals,
      multiple = TRUE,
      options = list(
        placeholder = "Select one or more values to filter the plot",
        plugins = list("remove_button")
      )
    )
  })

  observeEvent(input$clearVizFilter, {
    if (!is.null(input$vizMetaFilterValues)) {
      shiny::updateSelectizeInput(session, "vizMetaFilterValues", selected = character())
    }
  }, ignoreInit = TRUE)

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

  output$symbolGroupColumnUI = renderUI({
    req(inherits(rvals$plotdf, "data.frame"))
    choices <- names(rvals$plotdf)
    choices <- choices[!is.na(choices) & nzchar(choices)]
    choices <- setdiff(choices, "rowid")
    if (length(choices) == 0) return(NULL)

    active_input <- tryCatch(as.character(input$symbolGroupColumn[[1]]), error = function(e) "")
    prior_selection <- pick_selected_value(rvals$symbolGroupColumn, choices, fallback = "")
    default_group <- if (!is.null(rvals$attrGroups)) as.character(rvals$attrGroups) else ""
    if (!nzchar(default_group) || !(default_group %in% choices)) {
      default_group <- choices[[1]]
    }
    selected_col <- if (length(active_input) > 0 && !is.na(active_input[[1]]) && active_input[[1]] %in% choices) {
      active_input[[1]]
    } else if (nzchar(prior_selection)) {
      prior_selection
    } else {
      default_group
    }

    selectInput(
      "symbolGroupColumn",
      label = bslib::popover(
        tagList(
          "Symbol metadata field",
          trigger = bsicons::bs_icon("info-circle", title = "Help")
        ),
        title = "Symbol metadata field",
        "Choose which metadata field controls marker symbols. Defaults to the current group column."
      ),
      choices = choices,
      selected = selected_col
    )
  })

  output$xvar2UI = renderUI({
    req(rvals$chem)
    selectInput('xvar2', 'X', rvals$chem, multiple = T)
  })

  output$yvar2UI = renderUI({
    req(rvals$chem)
    selection <- resolve_multiplot_y_selection(
      all_vars = rvals$chem,
      x_vars = input$xvar2,
      y_vars = input$yvar2
    )
    selectInput(
      'yvar2',
      'Y',
      choices = selection$choices,
      multiple = T,
      selected = selection$selected
    )
  })

  observeEvent(input$xvar2, {
    req(rvals$chem)
    selection <- resolve_multiplot_y_selection(
      all_vars = rvals$chem,
      x_vars = input$xvar2,
      y_vars = input$yvar2
    )
    updateSelectInput(
      session,
      "yvar2",
      choices = selection$choices,
      selected = selection$selected
    )
  }, ignoreInit = TRUE)

  observeEvent(plotly::event_data("plotly_selected", source = "A"), {
    req(plot_df_for_display())
    plotly_select <- plotly::event_data("plotly_selected", source = "A")
    if (is.null(plotly_select) || !("key" %in% names(plotly_select))) {
      selected_plot_keys(character())
      rvals$brushSelected <- NULL
      return(invisible(NULL))
    }
    keys <- unique(as.character(plotly_select$key))
    selected_plot_keys(keys)
    rvals$brushSelected <- plot_df_for_display() %>%
      dplyr::filter(as.character(rowid) %in% keys)
  }, ignoreNULL = FALSE)

  observeEvent(plot_df_for_display(), {
    display_df <- plot_df_for_display()
    if (!inherits(display_df, "data.frame") || !("rowid" %in% names(display_df))) {
      selected_plot_keys(character())
      rvals$brushSelected <- NULL
      return(invisible(NULL))
    }
    display_keys <- unique(as.character(display_df$rowid))
    keys <- intersect(selected_plot_keys(), display_keys)
    selected_plot_keys(keys)
    if (length(keys) == 0) {
      rvals$brushSelected <- NULL
      return(invisible(NULL))
    }
    rvals$brushSelected <- display_df %>%
      dplyr::filter(as.character(rowid) %in% keys)
  }, ignoreInit = FALSE)

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
    inputList = c("xvar","yvar","xvar2","yvar2","data.src","Conf","int.set","use_symbols","symbolGroupColumn","show_point_labels","pointLabelColumn")
    for(i in inputList){
      rvals[[i]] = tryCatch(input[[i]],error = function(e)return(NULL))
    }
  })

  # plot
  output$plot <- plotly::renderPlotly({
    display_df <- plot_df_for_display()
    validate(need(inherits(display_df, "data.frame"), "Waiting for plot data"))
    req(input$xvar %in% names(display_df))
    req(input$yvar %in% names(display_df))
    req(rvals$attrGroups)
    validate(need(nrow(display_df) > 0, "No rows match the current visualization filter."))
    p <- tryCatch(
      mainPlot(
        plotdf = display_df,
        xvar = input$xvar,
        yvar = input$yvar,
        attrGroups = rvals$attrGroups,
        Conf = input$Conf,
        int.set = input$int.set,
        theme = input$plot_theme,
        use_symbols = isTRUE(input$use_symbols),
        symbol_col = input$symbolGroupColumn,
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

  output$multiplotUI = renderUI({
    req(rvals$multiplot)
    if (isTRUE(multiplot_mode())) {
      plotly::plotlyOutput("multiplotPlotly", width = "100%", height = paste0(multiplot_height(), "px"))
    } else {
      plotOutput("multiplotStatic", width = "100%", height = paste0(multiplot_height(), "px"))
    }
  })

  output$multiplotPlotly = plotly::renderPlotly({
    req(rvals$multiplot)
    req(isTRUE(multiplot_mode()))
    req(inherits(rvals$multiplot, "plotly"))
    rvals$multiplot
  })

  output$multiplotStatic = renderPlot({
    req(rvals$multiplot)
    req(!isTRUE(multiplot_mode()))
    req(inherits(rvals$multiplot, "ggplot"))
    rvals$multiplot
  }, width = "auto", height = function() multiplot_height())

  observeEvent(input$cancelMultiplotBuild, {
    active_request_id <- multiplot_active_request_id()
    if (is.na(active_request_id)) return(invisible(NULL))
    multiplot_cancelled_request_id(active_request_id)
    multiplot_active_request_id(NA_integer_)
    hide_multiplot_loading()
    mynotification("Cancelled multiplot build request.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$updateMultiplot, {
    request_id <- as.integer(multiplot_request_counter()) + 1L
    multiplot_request_counter(request_id)
    multiplot_active_request_id(request_id)
    multiplot_cancelled_request_id(NA_integer_)
    show_multiplot_loading()
    clear_loader_if_active <- function() {
      if (identical(multiplot_active_request_id(), request_id)) {
        multiplot_active_request_id(NA_integer_)
        hide_multiplot_loading()
      }
      invisible(NULL)
    }

    if (!inherits(rvals$selectedData, "data.frame") || nrow(rvals$selectedData) == 0) {
      mynotification("No data available for multiplot.", type = "warning")
      clear_loader_if_active()
      return(invisible(NULL))
    }
    axis_check <- validate_multiplot_axes(input$xvar2, input$yvar2)
    if (!isTRUE(axis_check$ok)) {
      mynotification(axis_check$message, type = "warning")
      clear_loader_if_active()
      return(invisible(NULL))
    }

    selected_data <- rvals$selectedData
    attr_group <- rvals$attrGroups
    point_size <- input$ptsize
    use_interactive <- interactive_mode(input$interactive)
    use_theme <- input$plot_theme
    use_height <- as.integer(input$plotHeight)

    if (!isTRUE(async_multiplot_enabled)) {
      ok <- tryCatch({
        quietly(label = "multiplot",{
          rvals$multiplot = multiplot(
            selectedData = selected_data,
            attrGroups = attr_group,
            xvar  = axis_check$x,
            yvar = axis_check$y,
            ptsize = point_size,
            interactive = use_interactive,
            theme = use_theme
          )
        })
        multiplot_mode(use_interactive)
        multiplot_height(use_height)
        TRUE
      }, error = function(e) {
        mynotification(paste0("Unable to build multiplot: ", conditionMessage(e)), type = "error")
        rvals$multiplot <- NULL
        FALSE
      })
      clear_loader_if_active()
      if (!isTRUE(ok)) return(invisible(NULL))
      return(invisible(NULL))
    }

    promise <- promises::future_promise({
      multiplot(
        selectedData = selected_data,
        attrGroups = attr_group,
        xvar = axis_check$x,
        yvar = axis_check$y,
        ptsize = point_size,
        interactive = use_interactive,
        theme = use_theme
      )
    })
    promise <- promises::then(
      promise,
      onFulfilled = function(multiplot_obj) {
        is_cancelled <- identical(multiplot_cancelled_request_id(), request_id)
        is_active <- identical(multiplot_active_request_id(), request_id)
        if (is_cancelled || !is_active) return(invisible(NULL))
        rvals$multiplot <- multiplot_obj
        multiplot_mode(use_interactive)
        multiplot_height(use_height)
        clear_loader_if_active()
        invisible(NULL)
      },
      onRejected = function(e) {
        if (!identical(multiplot_active_request_id(), request_id)) return(invisible(NULL))
        mynotification(paste0("Unable to build multiplot: ", conditionMessage(e)), type = "error")
        rvals$multiplot <- NULL
        clear_loader_if_active()
        invisible(NULL)
      }
    )
    multiplot_build_promise(promise)
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
