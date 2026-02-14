


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
        fluidRow(uiOutput('multiplot'))
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
visualizeAssignServer = function(input, output, session, rvals, credentials, con) {
  selected_plot_keys <- shiny::reactiveVal(character())

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
        rvals$plotVars = rvals$pca$x %>% colnames()
      } else if (input$data.src == 'linear discriminants') {
        validate(need(nrow(rvals$LDAdf) > 0, "No LDA results"))
        rvals$plotdf = tryCatch(rvals$LDAdf,error = function(e) {
          mynotification("No LDA results",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = rvals$LDAdf %>% colnames() %>% .[which(!. %in% rvals$attrs)]
      } else if (input$data.src == 'UMAP') {
        validate(need(nrow(rvals$umapdf) > 0, "No UMAP results"))
        rvals$plotdf = tryCatch(rvals$umapdf,error = function(e) {
          mynotification("No UMAP results",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = c("V1","V2")
      } else {
        req(nrow(rvals$selectedData) > 0)
        rvals$plotdf = tryCatch(rvals$selectedData,error = function(e) {
          mynotification("No data",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = rvals$chem
      }
      if(is.data.frame(rvals$plotdf) && nrow(rvals$plotdf) > 0 && !("rowid" %in% names(rvals$plotdf))){
        rvals$plotdf = rvals$plotdf %>% tibble::rowid_to_column("rowid")
      }
    })
  })

  output$xvarUI = renderUI({
    req(rvals$plotVars)
    selectInput('xvar', 'X', rvals$plotVars, selected = rvals$plotVars[1])
  })

  output$yvarUI = renderUI({
    req(rvals$plotVars)
    selectInput('yvar', 'y', rvals$plotVars, selected = rvals$plotVars[2])
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
    inputList = c("xvar","yvar","xvar2","yvar2","data.src","Conf","int.set")
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
    # quietly(label = "plotting data",{
    p =  mainPlot(plotdf = rvals$plotdf,xvar = input$xvar,yvar = input$yvar,attrGroups = rvals$attrGroups,Conf = input$Conf, int.set = input$int.set, theme = input$plot_theme)
    # })
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
    req(nrow(rvals$selectedData) > 0)
    req(input$xvar2)
    quietly(label = "multiplot",{

      rvals$multiplot = multiplot(selectedData = rvals$selectedData,attrGroups = rvals$attrGroups,xvar  = input$xvar2, yvar = input$yvar2,ptsize = input$ptsize, interactive = input$interactive, theme = input$plot_theme)
    })

    output$multiplot = renderUI({
      req(rvals$multiplot)
      if(input$interactive){
        plotly::renderPlotly(
          rvals$multiplot,
        )
      } else {
        renderPlot(
          rvals$multiplot,
          width = "auto",
          height = input$plotHeight
        )
      }
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
    })
  })

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
