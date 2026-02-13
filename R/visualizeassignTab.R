


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
            textInput('NewGroup', label = 'Enter new group designation')
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

  observeEvent(input$`plotly_selected-A`, {
    req(rvals$plotdf)
    plotlySelect <<- plotly::event_data("plotly_selected")
    print(plotlySelect)
    if (length(plotlySelect) > 0) {
      rvals$brushSelected = rvals$plotdf %>%
        dplyr::filter(as.character(rowid) %in% as.character(plotlySelect$key))
    }
  })

  observeEvent(input$Change, {
    req(rvals$brushSelected)
    req(rvals$plotdf)
    req(rvals$selectedData)
    quietly(label = "change group assignment",{

      rowid = rvals$brushSelected$rowid
      replaceCell(rowid = rowid,col = rvals$attrGroups,value = input$NewGroup, rvals = rvals, con = con, credentials = credentials, input = input, output = output, session = session)

      rvals$brushSelected = rvals$plotdf %>%
        dplyr::filter(as.character(rowid) %in% as.character(plotlySelect$key))

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
        renderTable(rvals$brushSelected)
      }
    })
  })
  })

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
