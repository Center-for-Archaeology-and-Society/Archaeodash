


#' UI elements for visualization and group reassignment
#'
#' @return
#' @export
#'
#' @examples
visualizeassignTab = function() {
  tabPanel(
    title = "Visualize & Assign",
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
              choices = c('elements', 'principal components'),
              selected = 'elements'
            ),
            uiOutput('xvarUI'),
            uiOutput('yvarUI'),
          ),
          column(
            3,
            offset = 0.5,

            checkboxInput('Conf', 'Confidence Elipse', value =
                            TRUE),
            sliderInput(
              'int.set',
              'Set Confidence Interval',
              min = 0.80,
              max = 0.99,
              step = 0.01,
              value = 0.90
            )
          ),
          column(
            3,
            offset = 0.5,
            br(),
            actionButton('Change', 'Change Group Assignment'),
            textInput('NewGroup', label = 'Enter new group designation')
          ),
          column(3,
                 offset = 0.5,
                 br())
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
          column(2, actionButton("updateMultiplot", "update")),
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
#' @param input
#' @param output
#' @param session
#' @param rvals
#'
#' @return
#' @export
#'
#' @examples
visualizeAssignServer = function(input, output, session, rvals) {

  observeEvent({
    rvals$selectedData
    input$data.src
    rvals$pcadf
    rvals$attrGroups
  },{
    if (input$data.src == 'principal components') {
      rvals$plotdf = tryCatch(rvals$pcadf,error = function(e) {
        shiny::showNotification("No PCA results",type = "warning")
        return(tibble::tibble())
      })
      rvals$plotVars = rvals$pca$x %>% colnames()
    } else {
      rvals$plotdf = tryCatch(rvals$selectedData,error = function(e) {
        shiny::showNotification("No data",type = "warning")
        return(tibble::tibble())
      })
      rvals$plotVars = rvals$chem
    }
  })

  output$xvarUI = renderUI({
    req(rvals$selectedData)
    selectInput('xvar', 'X', rvals$plotVars, selected = rvals$plotVars[1])
  })

  output$yvarUI = renderUI({
    req(rvals$selectedData)
    selectInput('yvar', 'y', rvals$plotVars, selected = rvals$plotVars[2])
  })

  output$xvar2UI = renderUI({
    req(rvals$selectedData)
    selectInput('xvar2', 'X', rvals$chem)
  })

  output$yvar2UI = renderUI({
    req(rvals$selectedData)
    req(input$xvar2)
    choices = setdiff(rvals$chem,input$xvar2)
    selectInput(
      'yvar2',
      'Y',
      choices = choices,
      multiple = T,
      selected = choices
    )
  })

  observeEvent(input$`plotly_selected-A`, {
    plotlySelect <<- plotly::event_data("plotly_selected")
    if (length(plotlySelect) > 0) {
      rvals$brushSelected = rvals$plotdf %>%
        dplyr::filter(rowid %in% plotlySelect$key)
    }
  })

  observeEvent(input$Change, {
    req(rvals$brushSelected)
    quietly({
      new = rvals$selectedData %>%
        dplyr::inner_join(rvals$brushSelected) %>%
        dplyr::mutate(!!as.name(rvals$attrGroups) := input$NewGroup)
      old = rvals$selectedData %>%
        dplyr::filter(!rowid %in% new$rowid)
      rvals$selectedData = dplyr::bind_rows(new, old) %>%
        dplyr::arrange(rowid) %>%
        dplyr::mutate_at(dplyr::vars(tidyselect::all_of(rvals$attrGroups)), factor)
      if(isFALSE(is.null(rvals$membershipProps)))
        rvals$membershipProbs[['GroupVal']] = rvals$selectedData[[rvals$attrGroups]]
    })
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  })

  # plot
  output$plot <- plotly::renderPlotly({
    req(rvals$plotdf)
    p1 <-
      ggplot2::ggplot(rvals$plotdf,
                      ggplot2::aes(
                        x = !!as.name(input$xvar),
                        y = !!as.name(input$yvar),
                        color = !!as.name(rvals$attrGroups),
                        shape = !!as.name(rvals$attrGroups),
                        key = rowid
                      )) +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = input$xvar,
        y = input$yvar,
        color = rvals$attrGroups,
        shape = rvals$attrGroups
      )
    if (input$Conf) {
      n = rvals$plotdf[[rvals$attrGroups]] %>% unique %>% length()
      if (n > 10) {
        showNotification("too many group members to plot confidence ellipses")
      } else {
        p1 <- p1 + ggplot2::stat_ellipse(level = input$int.set)
      }
    }
    suppressWarnings({
      plotly::ggplotly(p1) %>% plotly::layout(dragmode = 'lasso')
    })
  })

  output$brush <- renderUI({
    if (is.null(rvals$brushSelected)) {
      p("Click and drag events (i.e., select/lasso) appear here (double-click to clear)")
    } else {
      renderTable(rvals$brushSelected[,rvals$attrs])
    }
  })

  #### multiplots ####

  output$multiplotUI = renderUI({
    plotOutput("multiplot",
               width = "auto",
               height = input$plotHeight)
  })

  observeEvent(input$updateMultiplot, {
    req(rvals$selectedData)
    req(input$xvar2)
    p = rvals$selectedData %>%
      dplyr::select(tidyselect::all_of(c(rvals$attrGroups,input$xvar2,input$yvar2))) %>%
      tidyr::pivot_longer(-tidyselect::all_of(c(input$xvar2, rvals$attrGroups))) %>%
      ggplot2::ggplot(ggplot2::aes(
        y = !!as.name(input$xvar2),
        x = value,
        color = !!as.name(rvals$attrGroups)
      )) +
      ggplot2::geom_point(size = input$ptsize) +
      ggplot2::xlab("") +
      ggplot2::theme_bw(base_size = 14)
    if (length(input$yvar2) > 1) {
      p = p +
        ggplot2::facet_wrap( ~ name, scales = "free", strip.position = "left") +
        ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = '#404040'),
          strip.text = ggplot2::element_text(color = "white")
        ) +
        ggplot2::coord_flip()
    } else {
      p = p + ggplot2::xlab(input$yvar2) +
        ggplot2::coord_flip()
    }
    rvals$multiplot = p
  })

  output$multiplot = renderPlot({
    req(rvals$multiplot)
    rvals$multiplot
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
