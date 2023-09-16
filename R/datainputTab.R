#' UI elements for Data Input tab
#'
#' @return
#' @export
#'
#' @examples
datainputTab = function() {
  tagList(
    fileInput("file1", "Choose File (csv, xlsx or other supported format)"),
    uiOutput("attr"),
    uiOutput("subSelect"),
    uiOutput("chem"),
    uiOutput("actionUI"),
    uiOutput("resetUI"),
    br(),
    hr(),
    uiOutput('newCol'),
    br(),
    hr()
  )
}

#' Data Input
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
dataInputServer = function(input, output, session, rvals) {
  observeEvent(input$file1, {
    print("importing file")
    if (!is.null(input$file1)) {
      rvals$importedData = rvals$selectedData = quietly(rio::import(input$file1$datapath, setclass = 'tibble')) %>%
        setNames(janitor::make_clean_names(names(.),case = 'none'))
    }
  })

  # Render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    req(rvals$selectedData)
    df <- rvals$selectedData
    # Remove numeric columns from default selection
    nums1 <- unlist(lapply(df, is.numeric))
    items = names(df[,!nums1])
    # Set names as all columns in datatable
    items.all <- names(df)
    names(items.all) = items.all
    names(items) = items
    tagList(
      selectInput(
        "attr",
        "Select all of the attribute variables you want to display:",
        items.all,
        multiple = TRUE,
        selected = items
      ),
      selectInput(
        "attrGroups",
        "Select descriptive/group column:",
        items.all,
        multiple = F
      )
    )
  })

  # Render select lookup for choosing groups to include
  output$subSelect <- renderUI({
    req(rvals$selectedData)
    req(input$attrGroups)
    items.all = quietly(rvals$selectedData[[input$attrGroups]] %>% unique %>% sort)
    tagList(
      selectInput(
        "attrGroupsSub",
        "Select groups to include",
        choices = items.all,
        multiple = TRUE,
        selected = items.all
      )
    )
  })

  # Render multi-select lookup for choosing chemical concentration columns
  output$chem <- renderUI({
    df <- rvals$selectedData
    if (is.null(df))
      return(NULL)
    # Only include numeric columns in default selection
    nums <- unlist(lapply(df, is.numeric))
    items = names(df[, nums])
    # Set names as all columns in datatable
    items.all <- names(df)
    names(items) = items
    names(items.all) = items.all
    selectInput(
      "chem",
      "Select all of the element concentrations:",
      items.all,
      multiple = TRUE,
      selected = items
    )
  })

  # Render button to update datatable based on variable selections
  output$actionUI <- renderUI({
    req(input$file1)
    actionButton("action", "Press to confirm selections")
  })

  output$resetUI <- renderUI({
    req(rvals$selectedData)
    rvals$selectedData
    actionButton("reset", "Reset all to original")
  })

  observeEvent(input$reset,{
    showModal(modalDialog(title = "Confirm",shiny::p("Press to confirm. All changes will be lost"),footer = tagList(actionButton("confirmReset","confirm"),modalButton("cancel")),easyClose = T))
  })

  observeEvent(input$confirmReset,{
    removeModal()
    rvals$selectedData = rvals$importedData
  })

  # create subset data frame
  observeEvent(input$action, {
    req(rvals$selectedData)
    req(input$attr)
    req(input$chem)
    isolate({
    rvals$chem = input$chem
    rvals$attrGroups = input$attrGroups
    rvals$attr = input$attr
    rvals$attrs = unique(c('rowid',rvals$attr,rvals$attrGroups))
    rvals$selectedData = quietly(
      rvals$selectedData %>%
        dplyr::select(-tidyselect::any_of('rowid')) %>%
        tibble::rowid_to_column() %>%
        dplyr::select(
          rowid,
          tidyselect::all_of(input$attr),
          tidyselect::all_of(input$attrGroups),
          tidyselect::all_of(input$chem)
        ) %>%
        dplyr::mutate_at(dplyr::vars(input$attrGroups), factor)
    )
    if(isTRUE(is.null(input$attrGroupsSub))){
      showNotification("Cannot proceed without any groups selected",type = "error")
    } else {
      rvals$selectedData = quietly(rvals$selectedData %>%
                                     dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub))
    }
    })
  })

  output$newCol = renderUI({
    req(rvals$selectedData)
    actionButton('addNewCol', "Add New Column")
  })

  observeEvent(input$addNewCol, {
    showModal(modalDialog(
      textInput('createGroup', "New Group Name", value = "cluster"),
      textInput('createGroupVal', "New Group Default Value", value = "1"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createSubmit", "Submit")
      )
    ))
  })

  observeEvent(input$createSubmit,{
    removeModal()
    if(isTRUE(is.null(input$createGroup))) newCol = "cluster" else newCol = input$createGroup
    if(isTRUE(is.null(input$createGroupVal))) val = "1" else val = input$createGroupVal
    rvals$selectedData = quietly(
      rvals$selectedData %>%
        dplyr::select(-tidyselect::any_of(newCol)) %>%
        dplyr::mutate(!!as.name(newCol) := factor(val))
    )
  })

}
