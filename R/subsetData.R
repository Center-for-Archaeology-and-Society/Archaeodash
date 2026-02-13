#' Subset Data UI
#'
#' @return UI
#' @export
#'
#' @examples
#' subsetData()
subsetData = function(){
  showModal(modalDialog(
    title = "Subset dataset",
    uiOutput("subsetDFUI"),
    uiOutput("subsetAttrUI"),
    uiOutput("subsetElemsUI"),
    uiOutput("subsetGroupUI"),
    uiOutput("subsetChoicesUI"),
    textInput(inputId = "subsetName",label = "Choose name for new dataset",value = "subset"),
    footer = tagList(
      modalButton("cancel"),
      actionButton("subsetSubmit","submit")
    )
  ))
}

#' Subset data server
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
#' subsetDataServer(input,output,session,rvals)
subsetDataServer = function(input,output,session,rvals){

  output$subsetDFUI = renderUI({
    req(rvals$df)
    selectInput("subsetDF","Choose dataset to subset",choices = names(rvals$df))
  })

  output$subsetAttrUI = renderUI({
    req(rvals$df)
    nms <- rvals$df[[input$subsetDF]]$attrData %>% names()
    tagList(
      selectInput(
        "subsetAttr",
        "Select all of the attribute variables you want to display:",
        nms,
        multiple = TRUE,
        selected = nms
      ),
      selectInput(
        "subsetAttrGroups",
        "Select descriptive/grouping columns:",
        nms,
        multiple = TRUE,
        selected = nms
      )
    )
  })

  output$subsetElemsUI = renderUI({
    req(rvals$df)
    nms = rvals$df[[input$subsetDF]]$chemicalData %>% names()
    selectInput("subsetElems","Select all of the element concentrations:",
                nms,
                multiple = TRUE,
                selected = nms)
  })

  output$subsetGroupUI = renderUI({
    req(rvals$df)
    selectInput("subsetGroup","Choose group to subset by:",
                input$subsetAttrGroups,
                multiple = FALSE)
  })

  output$subsetChoicesUI = renderUI({
    req(rvals$df)
    choices = tryCatch(rvals$df[[input$subsetDF]]$attrData %>% dplyr::pull(input$subsetGroup) %>% unique() %>% sort(),error = function(e){
      print("subsetChoicesUI error")
      return("")
    })
    selectInput("subsetChoice","Choose the groups you wish to include:",
                choices,
                multiple = TRUE,
                selected = choices)
  })

  observeEvent(input$subsetSubmit,{
    removeModal()
    rvals$df[[input$subsetName]]$attrData = rvals$df[[input$subsetDF]]$attrData %>%
      dplyr::select(tidyselect::any_of(input$subsetAttr)) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::any_of(input$subsetAttrGroups)),factor)
    index = which(
      rvals$df[[input$subsetName]]$attrData[[input$subsetGroup]] %in% input$subsetChoice
      )

    rvals$df[[input$subsetName]]$attrData = rvals$df[[input$subsetName]]$attrData %>%
      dplyr::slice(index)

    if(length(input$subsetAttrGroups) == 0){
      rvals$df[[input$subsetName]]$attrData = rvals$df[[input$subsetName]]$attrData %>%
        dplyr::mutate(cluster = factor("none"))
    }

    rvals$df[[input$subsetName]]$chemicalData =
      rvals$df[[input$subsetDF]]$chemicalData %>%
      dplyr::select(tidyselect::any_of(input$subsetElems)) %>%
      dplyr::slice(index)

  })
}

#' Subset Module UI
#'
#' @param id module ID
#' @param label module label
#'
#' @return module
#' @export
#'
#' @examples
#' subsetModUI("subset")
subsetModUI = function(id,label = "subset"){
  ns <- NS(id)
  uiOutput(ns('subsetButton'))
}

#' Subset Module Server
#'
#' @param id module ID
#' @param rvals reactive values object
#'
#' @return module server
#' @export
#'
#' @examples
#' subsetModServer("subset",rvals)
subsetModServer <- function(id,rvals) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      # subset UI
      output$subsetButton = renderUI({
        req(rvals$df)
        actionButton(ns("subsetDFButton"),"subset selected dataset")
      })

      # subset data
      observeEvent(input$subsetDFButton,{
        subsetData()
      })

    })
}

#' Choose dataset UI
#'
#' @param id module ID
#' @param label module label
#'
#' @return module
#' @export
#'
#' @examples
#' chooseDFUI("chooseDF")
chooseDFUI = function(id, label = "choose DF"){
  ns <- NS(id)
  uiOutput(ns('availableDFsUI'))
}

#' Choose dataset server
#'
#' @param id module ID
#' @param rvals reactive values object
#'
#' @return module server
#' @export
#'
#' @examples
#' chooseDFServer("chooseDF",rvals)
chooseDFServer = function(id,rvals) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- NS(id)

      output$availableDFsUI = renderUI({
        req(rvals$importedData)
        req(rvals$df)
        selectInput(inputId = ns("selectedDF"),label = "Choose from available datasets",choices = names(rvals$df))
      })

    })
}
