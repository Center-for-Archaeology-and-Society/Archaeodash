


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
              choices = c('elements', 'principal components','canonical discriminants'),
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
visualizeAssignServer = function(input, output, session, rvals) {

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
      } else if (input$data.src == 'canonical discriminants') {
        validate(need(nrow(rvals$CDAdf) > 0, "No CDA results"))
        rvals$plotdf = tryCatch(rvals$CDAdf,error = function(e) {
          mynotification("No CDA results",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = rvals$CDAdf %>% colnames() %>% .[which(!. %in% rvals$attrs)]
      } else {
        req(nrow(rvals$selectedData) > 0)
        rvals$plotdf = tryCatch(rvals$selectedData,error = function(e) {
          mynotification("No data",type = "warning")
          return(tibble::tibble())
        })
        rvals$plotVars = rvals$chem
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

  observeEvent(input$`plotly_selected-A`, {
    req(rvals$plotdf)
    plotlySelect <<- plotly::event_data("plotly_selected")
    if (length(plotlySelect) > 0) {
      rvals$brushSelected = rvals$plotdf %>%
        dplyr::filter(rowid %in% plotlySelect$key)
    }
  })

  observeEvent(input$Change, {
    req(rvals$brushSelected)
    req(rvals$plotdf)
    req(rvals$selectedData)
    quietly(label = "change group assignment",{
      new = rvals$selectedData %>%
        dplyr::inner_join(rvals$brushSelected) %>%
        dplyr::mutate(!!as.name(rvals$attrGroups) := input$NewGroup)
      old = rvals$selectedData %>%
        dplyr::filter(!rowid %in% new$rowid)
      rvals$selectedData = dplyr::bind_rows(new, old) %>%
        dplyr::arrange(rowid) %>%
        dplyr::mutate_at(dplyr::vars(tidyselect::all_of(rvals$attrGroups)), factor)
      if(nrow(rvals$pcadf) > 0){
        rvals$pcadf[,rvals$attrs] = rvals$selectedData[,rvals$attrs]
      }
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
    validate(need(inherits(rvals$plotdf,"data.frame"),"Waiting for plot data"))
    req(nrow(rvals$plotdf) > 0)
    req(input$xvar %in% names(rvals$plotdf))
    req(input$yvar %in% names(rvals$plotdf))
    req(rvals$attrGroups)
    quietly(label = "plotting data",{
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
        ) +
        ggplot2::scale_color_viridis_d()
      if (input$Conf) {
        n = rvals$plotdf[[rvals$attrGroups]] %>% unique %>% length()
        if (n > 5) {
          mynotification("too many group members to plot confidence ellipses")
        } else {
          # p1 <- p1 + ggplot2::stat_ellipse(level = input$int.set)
        }
      }
      suppressWarnings({
        plotly::ggplotly(p1) %>% plotly::layout(dragmode = 'lasso')
      })
    })
  })

  output$brush <- renderUI({
    req(rvals$brushSelected)
    quietly(label = "brush UI",{
      if (is.null(rvals$brushSelected)) {
        p("Click and drag events (i.e., select/lasso) appear here (double-click to clear)")
      } else {
        if(isTRUE(input$data.src == "canonical discriminants")){
          renderTable(rvals$brushSelected)
        } else {
          renderTable(rvals$brushSelected[,rvals$attrs])
        }
      }
    })
  })

  #### multiplots ####

  output$multiplotUI = renderUI({
    plotOutput("multiplot",
               width = "auto",
               height = input$plotHeight)
  })

  observeEvent(input$updateMultiplot, {
    req(nrow(rvals$selectedData) > 0)
    req(input$xvar2)
    quietly(label = "multiplot",{
      pdf1 = rvals$selectedData %>%
        dplyr::select(rowid,tidyselect::all_of(c(rvals$attrGroups,input$xvar2))) %>%
        tidyr::pivot_longer(tidyselect::all_of(input$xvar2), names_to = "xvar2", values_to = "elem1") %>%
        tidyr::unite(id, c('rowid',rvals$attrGroups), sep = "_", remove = F)
      pdf2 = rvals$selectedData %>%
        dplyr::select(rowid,tidyselect::all_of(c(rvals$attrGroups,input$yvar2))) %>%
        tidyr::pivot_longer(tidyselect::all_of(input$yvar2), names_to = "yvar2", values_to = "elem2") %>%
        tidyr::unite(id, c('rowid',rvals$attrGroups), sep = "_", remove = F)
      p = dplyr::full_join(pdf1,pdf2 %>% dplyr::select(-tidyselect::all_of(c('rowid',rvals$attrGroups))), by = 'id', relationship = "many-to-many") %>%
        dplyr::filter(elem1 != elem2) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = elem1,
          y = elem2,
          color = !!as.name(rvals$attrGroups)
        )) +
        ggplot2::geom_point(size = input$ptsize) +
        ggplot2::ylab("") +
        ggplot2::theme_bw(base_size = 14,) +
        ggplot2::scale_color_viridis_d() +
        ggplot2::theme(
          strip.background = ggplot2::element_rect(fill = '#404040'),
          strip.text = ggplot2::element_text(color = "white")
        )
      if(length(input$xvar2) > 1 || ((length(input$xvar2) == 1 & length(input$yvar2) == 1))){
        p = p +
          ggplot2::facet_grid(rows = dplyr::vars(yvar2), cols = dplyr::vars(xvar2), scales = 'free',switch = 'both') +
          ggplot2::xlab("")
      } else {
        p = p +
          ggplot2::facet_wrap(~yvar2, scales = 'free',strip.position = "left") +
          ggplot2::xlab(input$xvar2)
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
