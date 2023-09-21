#' UI elements for Data Input tab
#'
#' @return NULL
#' @export
#'
#' @examples
#' datainputTab()
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
      rvals$chem = NULL
      rvals$attrGroups = NULL
      rvals$attr = NULL
      rvals$attrs = NULL
      rvals$attrGroupsSub = NULL
      rvals$xvar = NULL
      rvals$xvar2 = NULL
      rvals$yvar = NULL
      rvals$yvar2 = NULL
      rvals$data.src = NULL
      rvals$Conf = NULL
      rvals$int.set = NULL
      rvals$unselectedData = NULL
      rvals$importedData = rvals$selectedData = quietly(rio::import(input$file1$datapath, setclass = 'tibble')) %>%
        setNames(janitor::make_clean_names(names(.),case = 'none'))
    }
  })

  # Render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    req(rvals$selectedData)
    df <- dplyr::bind_rows(rvals$selectedData,rvals$unselectedData)
    # Remove numeric columns from default selection
    nums1 <- unlist(lapply(df, is.numeric))
    items = names(df[,!nums1])
    # Set names as all columns in datatable
    items.all <- names(df)
    names(items.all) = items.all
    names(items) = items
    if(isTRUE(is.null(rvals[['attr']])))
      selection = items else
        selection = rvals[['attr']]
    if(isTRUE(is.null(rvals[['attrGroups']])))
      selection2 = items else
        selection2 = rvals[['attrGroups']]
    tagList(
      selectInput(
        "attr",
        "Select attribute variables you want to display:",
        items.all,
        multiple = TRUE,
        selected = selection
      ),
      selectInput(
        "attrGroups",
        "Select descriptive/group column:",
        items.all,
        multiple = F,
        selected = selection2[1]
      )
    )
  })

  # Render select lookup for choosing groups to include
  output$subSelect <- renderUI({
    req(rvals$selectedData)
    req(input$attrGroups)
    df <- dplyr::bind_rows(rvals$selectedData,rvals$unselectedData)
    items.all = quietly(df[[input$attrGroups]] %>% unique %>% sort)
    print("subSelect")
    print(rvals[['attrGroupsSub']])
    if(isTRUE(is.null(rvals[['attrGroupsSub']])))
      selection = items.all else
        selection = rvals[['attrGroupsSub']]
    tagList(
      selectInput(
        "attrGroupsSub",
        "Select groups to include",
        choices = items.all,
        multiple = TRUE,
        selected = selection
      )
    )
  })

  # Render multi-select lookup for choosing chemical concentration columns
  output$chem <- renderUI({
    req(rvals$selectedData)
    df <- dplyr::bind_rows(rvals$selectedData,rvals$unselectedData)
    if (is.null(df))
      return(NULL)
    # Only include numeric columns in default selection
    nums <- unlist(lapply(df, is.numeric))
    items = names(df[, nums]) %>% .[which(. != "rowid")]
    # Set names as all columns in datatable
    items.all <- names(df)
    names(items) = items
    names(items.all) = items.all
    selectInput(
      "chem",
      "Select element concentrations:",
      items.all,
      multiple = TRUE,
      selected = items
    )
  })

  # Render button to update datatable based on variable selections
  output$actionUI <- renderUI({
    req(input$file1)
    tagList(
      checkboxInput("runPCA","check to run PCA", value = F),
      # checkboxInput("runCDA","check to run CDA", value = F),
      actionButton("action", "Press to confirm selections")
    )
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
    rvals$chem = NULL
    rvals$attrGroups = NULL
    rvals$attr = NULL
    rvals$attrs = NULL
    rvals$attrGroupsSub = NULL
    rvals$selectedData = rvals$importedData
  })

  # create subset data frame
  observeEvent(input$action, {
    req(rvals$selectedData)
    req(input$attr)
    req(input$chem)
    rvals$chem = input$chem
    rvals$attrGroups = input$attrGroups
    rvals$attr = input$attr
    rvals$attrs = unique(c('rowid',rvals$attr,rvals$attrGroups))
    rvals$attrGroupsSub = input$attrGroupsSub
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
    if(isTRUE(inherits(rvals$unselectedData,"data.frame"))){
      rvals$selectedData = dplyr::bind_rows(rvals$selectedData,rvals$unselectedData) %>% dplyr::arrange(rowid)
    }
    rvals$selectedData =
      rvals$selectedData %>%
      dplyr::select(-tidyselect::any_of('rowid')) %>%
      tibble::rowid_to_column() %>%
      dplyr::select(
        rowid,
        tidyselect::all_of(input$attr),
        tidyselect::all_of(input$attrGroups),
        tidyselect::all_of(input$chem)
      ) %>%
      dplyr::mutate_at(dplyr::vars(input$attrGroups), factor) %>%
      dplyr::mutate_at(dplyr::vars(input$chem), quietly(as.numeric))
    if(isTRUE(is.null(input$attrGroupsSub))){
      showNotification("Cannot proceed without any groups selected",type = "error")
    } else {
      keep = quietly(rvals$selectedData %>%
                       dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub))
      discard = quietly(rvals$selectedData %>%
                          dplyr::filter(!rowid %in% keep$rowid))
      rvals$selectedData = keep
      rvals$unselectedData = discard
    }
    try({
    rvals$runPCA = input$runPCA
    rvals$runCDA = input$runCDA
    if(isTRUE(rvals$runPCA)) showNotification("Running PCA")
    if(isTRUE(rvals$runCDA)) showNotification("Running CDA")
    })
    showNotification("updated",duration = 3)
  })

  observeEvent(rvals$selectedData,{
    print("restoring state")
    restoreState(rvals = rvals,input = input,session = session)
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
    print("adding column")
    if(isTRUE(is.null(input$createGroup))) newCol = "cluster" else newCol = input$createGroup
    if(isTRUE(is.null(input$createGroupVal))) val = "1" else val = input$createGroupVal
    rvals$selectedData = quietly(
      rvals$selectedData %>%
        dplyr::select(-tidyselect::any_of(newCol)) %>%
        dplyr::mutate(!!as.name(newCol) := factor(val))
    )
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  })

}
