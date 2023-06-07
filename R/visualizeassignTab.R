

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
    tabsetPanel(id = "visualize",
                tabPanel(title = "visualize and select",
                         fluidRow(column(9,
                                         plotly::plotlyOutput('plot', width = '100%', height = '600px')
                         ),
                         column(3,
                                div(
                                  uiOutput('sel')),
                                style = "max-height: 600px !important; overflow: auto;"
                         )
                         ),
                         fluidRow(
                           column(
                             2,
                             chooseDFUI("vs"),
                             br(),
                             subsetModUI("vs"),
                             br(),
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
                             uiOutput('CodeUI'),

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
                             actionButton('addGroup',"Add New Column"),
                             br(),
                             actionButton('Change', 'Change Group Assignment'),
                             textInput('NewGroup', label = 'Enter new group designation')
                           ),
                           column(
                             3,
                             offset = 0.5,
                             br()
                           )
                         ),
                         uiOutput('brush')
                ),
                tabPanel(title = "multiplots",
                         chooseDFUI("mp"),
                         br(),
                         subsetModUI("mp"),
                         br(),
                         fluidRow(column(3,uiOutput('xvar2UI')),
                                  column(1),
                                  column(3,
                                         uiOutput('yvar2UI')
                                  ),
                                  column(1),
                                  column(3,
                                         uiOutput('Code2UI')
                                  )
                         ),
                         fluidRow(column(3,actionButton("updateMultiplot","update")),column(1),column(3,numericInput("plotHeight",label = "plot height in pixels",min = 500,max = 2000, value = 900, step = 50)),column(1),column(4,actionButton('savePlot',"Save Plot"))),
                         fluidRow(
                           uiOutput('multiplotUI')
                         )
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
visualizeAssignServer = function(input,output,session,rvals){

  chooseDFServer("vs",rvals)

  chooseDFServer("mp",rvals)

  subsetModServer("vs",rvals)

  subsetModServer("mp",rvals)

  output$sel <- renderUI({
    req(input$Code)
    req(rvals$df[[input$`vs-selectedDF`]]$attrData)
    vals = rvals$df[[input$`vs-selectedDF`]]$attrData[[input$Code]] %>% unique %>% sort
    checkboxGroupInput("groups",
                       "Groups to show:",
                       choices = vals,
                       selected = vals)
  })

  output$xvarUI = renderUI({
    req(rvals$df[[input$`vs-selectedDF`]]$chemicalData)
    if (input$data.src == 'principal components') {
      df = try(rvals$df[[input$`vs-selectedDF`]]$pcaResults$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$df[[input$`vs-selectedDF`]]$chemicalData, silent = T)
    }
    selectInput('xvar', 'X', names(df), selected = names(df)[1])
  })

  output$yvarUI = renderUI({
    req(rvals$df[[input$`vs-selectedDF`]]$chemicalData)
    if (input$data.src == 'principal components') {
      df = try(rvals$df[[input$`vs-selectedDF`]]$pcaResults$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$df[[input$`vs-selectedDF`]]$chemicalData, silent = T)
    }
    selectInput('yvar', 'y', names(df), selected = names(df)[2])
  })

  output$CodeUI = renderUI({
    selectInput('Code', 'GROUP', choices = colnames(rvals$df[[input$`vs-selectedDF`]]$attrData)[sapply(rvals$df[[input$`vs-selectedDF`]]$attrData, is.factor)])
  })

  output$xvar2UI = renderUI({
    req(rvals$df[[input$`vs-selectedDF`]]$chemicalData)
    selectInput('xvar2', 'X', names(rvals$df[[input$`vs-selectedDF`]]$chemicalData))
  })

  output$yvar2UI = renderUI({
    req(rvals$df[[input$`vs-selectedDF`]]$chemicalData)
    req(input$xvar2)
    choices = names(rvals$df[[input$`vs-selectedDF`]]$chemicalData)[which(names(rvals$df[[input$`vs-selectedDF`]]$chemicalData) != input$xvar2)]
    selectInput('yvar2', 'Y', choices = choices, multiple = T, selected = choices)
  })

  output$Code2UI = renderUI({
    choices = rvals$df[[input$`vs-selectedDF`]]$attrData[[input$Code]] %>% unique %>% sort
    tagList(
    selectInput('multigroup', 'GROUP', choices = colnames(rvals$df[[input$`vs-selectedDF`]]$attrData)[sapply(rvals$df[[input$`vs-selectedDF`]]$attrData, is.factor)]),
    selectInput('multifilter', 'Groups to show:', choices = choices, multiple = T, selected = choices)
    )
  })

  observeEvent({
    input$Code
    input$xvar
    input$yvar
    input$groups
    input$data.src
  }, {
    req(input$Code)
    req(input$xvar)
    req(input$yvar)
    req(input$groups)
    if (input$data.src == 'principal components') {
      df = try(rvals$df[[input$`vs-selectedDF`]]$pcaResults$x %>% dplyr::as_tibble(), silent = T)
    } else {
      df = try(rvals$df[[input$`vs-selectedDF`]]$chemicalData, silent = T)
    }
    rvals$plotlydf = tryCatch({
      df %>%
        dplyr::mutate(rowid = 1:dplyr::n()) %>%
        dplyr::select(rowid,
                      x = tidyselect::all_of(input$xvar),
                      y = tidyselect::all_of(input$yvar)) %>%
        dplyr::bind_cols(rvals$df[[input$`vs-selectedDF`]]$attrData %>% dplyr::select(group = tidyselect::all_of(input$Code))) %>%
        dplyr::filter(group %in% input$groups)
    },
    error = function(e) {
      showNotification("Error returning plot dataset")
      return(NULL)
    })
  })

  observeEvent(input$`plotly_selected-A`, {
    plotlySelect <<- plotly::event_data("plotly_selected")
    if (length(plotlySelect) > 0) {
      rvals$brushSelected = rvals$plotlydf %>%
        dplyr::filter(rowid %in% plotlySelect$key)
      rvals$attrBrush = rvals$df[[input$`vs-selectedDF`]]$attrData %>%
        tibble::rowid_to_column() %>%
        dplyr::filter(rowid %in% plotlySelect$key)
    }
  })

  observeEvent(input$addGroup,{

    # save state of inputs to update
    rvals$state$selected = input$`vs-selectedDF`
    rvals$state$type = input$data.src
    rvals$state$xvar = input$xvar
    rvals$state$yvar = input$yvar
    rvals$state$group = input$code
    rvals$state$elipse = input$Conf
    rvals$state$ci = input$int.set

    showModal(modalDialog(
      textInput('createGroup',"New Group Name",value = "cluster"),
      textInput('createGroupVal',"New Group Default Value",value = "1"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createSubmit", "Submit")
      )
    ))
  })

  observeEvent(input$createSubmit,{
    removeModal()
    rvals$df[[input$`vs-selectedDF`]]$attrData = rvals$df[[input$`vs-selectedDF`]]$attrData %>%
      dplyr::mutate(!!as.name(input$createGroup) := factor(input$createGroupVal))

    # update inputs from saved states
    updateSelectInput(session = session,inputId = "vs-selected",selected = rvals$state$selected)
    updateSelectInput(session = session, inputId = "data.src",selected = rvals$state$type)
    updateSelectInput(session = session, inputId = "xvar",selected = rvals$state$xvar)
    updateSelectInput(session = session, inputId = "yvar",selected = rvals$state$yvar)
    updateSelectInput(session = session, inputId = "code",selected = rvals$state$group)
    updateSelectInput(session = session, inputId = "Conf",selected = rvals$state$elipse)
    updateSelectInput(session = session, inputId = "int.set",selected = rvals$state$ci)
  })

  observeEvent(input$NewGroup,{
    if(stringr::str_detect(input$NewGroup,"[a-zA-z]|[0-9]") && isTRUE(nrow(rvals$brushSelected) > 0)){
      shinyjs::enable("Change")
    } else {
      shinyjs::disable("Change")
    }
  })

  observeEvent(input$Change, {
    rvals$state$selected = input$`vs-selectedDF`
    rvals$state$type = input$data.src
    rvals$state$xvar = input$xvar
    rvals$state$yvar = input$yvar
    rvals$state$group = input$code
    rvals$state$elipse = input$Conf
    rvals$state$ci = input$int.set
    req(rvals$brushSelected)
    new = rvals$df[[input$`vs-selectedDF`]]$attrData %>%
      dplyr::mutate(rowid = 1:dplyr::n()) %>%
      dplyr::filter(rowid %in% rvals$brushSelected$rowid) %>%
      dplyr::mutate(!!as.name(input$Code) := input$NewGroup)
    old = rvals$df[[input$`vs-selectedDF`]]$attrData %>%
      dplyr::mutate(rowid = 1:dplyr::n()) %>%
      dplyr::filter(!rowid %in% rvals$brushSelected$rowid)
    rvals$df[[input$`vs-selectedDF`]]$attrData = dplyr::bind_rows(new, old) %>%
      dplyr::arrange(rowid) %>%
      dplyr::select(-rowid) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::all_of(input$Code)),factor)
    updateSelectInput(session = session,inputId = "vs-selected",selected = rvals$state$selected)
    updateSelectInput(session = session, inputId = "data.src",selected = rvals$state$type)
    updateSelectInput(session = session, inputId = "xvar",selected = rvals$state$xvar)
    updateSelectInput(session = session, inputId = "yvar",selected = rvals$state$yvar)
    updateSelectInput(session = session, inputId = "code",selected = rvals$state$group)
    updateSelectInput(session = session, inputId = "Conf",selected = rvals$state$elipse)
    updateSelectInput(session = session, inputId = "int.set",selected = rvals$state$ci)
  })

  # plot
  output$plot <- plotly::renderPlotly({
    req(rvals$plotlydf)
    p1 <-
      ggplot2::ggplot(rvals$plotlydf,
                      ggplot2::aes(
                        x = x,
                        y = y,
                        color = group,
                        shape = group,
                        key = rowid
                      )) +
      ggplot2::geom_point() +
      ggplot2::labs(
        x = input$xvar,
        y = input$yvar,
        color = input$Code,
        shape = input$Code
      )
    if (input$Conf) {
      n = rvals$plotlydf$group %>% unique %>% length()
      if (n > 10) {
        showNotification("too many group members to plot confidence ellipses")
      } else {
        p1 <- p1 + ggplot2::stat_ellipse(level = input$int.set)
      }
    }
    plotly::ggplotly(p1) %>% plotly::layout(dragmode = 'lasso')
  })

  output$brush <- renderUI({
    if (is.null(rvals$attrBrush)) {
      p("Click and drag events (i.e., select/lasso) appear here (double-click to clear)")
    } else {
      renderTable(rvals$attrBrush)
    }
  })

  #### multiplots ####

  output$multiplotUI = renderUI({
    plotOutput("multiplot",width = "auto",height = input$plotHeight)
  })

  observeEvent(input$updateMultiplot,{
    req(rvals$df[[input$`mp-selectedDF`]]$chemicalData)
    req(input$xvar2)
    p = rvals$df[[input$`mp-selectedDF`]]$chemicalData %>%
      dplyr::bind_cols(rvals$df[[input$`mp-selectedDF`]]$attrData %>% dplyr::select(tidyselect::any_of(input$multigroup))) %>%
      dplyr::select(tidyselect::any_of(c(input$xvar2,input$yvar2,input$multigroup))) %>%
      dplyr::filter(!!as.name(input$multigroup) %in% input$multifilter) %>%
      tidyr::pivot_longer(-tidyselect::all_of(c(input$xvar2,input$multigroup))) %>%
      ggplot2::ggplot(ggplot2::aes(y = !!as.name(input$xvar2), x = value, color = !!as.name(input$multigroup))) +
      ggplot2::geom_point() +
      ggplot2::xlab("") +
      ggplot2::theme_bw(base_size = 14)
    if(length(input$yvar2) > 1){
      p = p +
        ggplot2::facet_wrap(~name, scales = "free", strip.position = "left") +
        ggplot2::theme(strip.background = ggplot2::element_rect(fill = '#404040'),
                       strip.text = ggplot2::element_text(color = "white")) +
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

  observeEvent(input$savePlot,{
    showModal(modalDialog(
      title = "Save Plot",
      textInput("plotfilename", "File name:", value = "ggplot.png"),
      numericInput("width", "Width (inches):", value = 7),
      numericInput("height", "Height (inches):", value = 5),
      numericInput("res", "Resolution (dpi):", value = 300),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("saveMultiPlot", "Save")
      )
    ))
  })

  output$saveMultiPlot <- downloadHandler(
    filename = function() {
      input$plotfilename
    },
    content = function(file) {
      ggplot2::ggsave(filename = file, plot = rvals$multiplot,
                      width = input$width, height = input$height, dpi = input$res)
    }
  )

}
