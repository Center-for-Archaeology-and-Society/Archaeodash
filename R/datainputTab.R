#' UI elements for Data Input tab
#'
#' @return NULL
#' @export
#'
#' @examples
#' datainputTab()
datainputTab = function() {
  tagList(
    fileInput("file1", "Choose File(s) (csv, xlsx or other supported format)",multiple = T),
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
      print(input$file1$datapath)
      rvals$importedData = rvals$selectedData = quietly(purrr::map_df(input$file1$datapath,function(fp)rio::import(fp, setclass = 'tibble') %>% dplyr:mutate(file = basename(fp)))) %>%
        setNames(janitor::make_clean_names(names(.),case = 'none')) %>%
        dplyr::select(-tidyselect::any_of('rowid')) %>%
        tibble::rowid_to_column()
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
      uiOutput("impute.options"),
      br(),
      uiOutput("transform.options"),
      br(),
      checkboxInput("runPCA","check to run PCA", value = F),
      # checkboxInput("runCDA","check to run CDA", value = F),
      actionButton("action", "Press to confirm selections", class = "mybtn")
    )
  })

  # Render options for data imputation
  output$impute.options <- renderUI({
    req(rvals$selectedData)
    radioButtons(
      "impute.method",
      label = ("Select Imputation Method"),
      choices = list(
        "None" = "none",
        "Random Forest" = "rf",
        "Predictive Mean Matching" = "pmm",
        "Weighted Predictive Mean Matching" = "midastouch"
      ),
      selected = "none"
    )
  })

  # Render options for data transformation
  output$transform.options <- renderUI({
    req(req(rvals$selectedData))
    radioButtons(
      "transform.method",
      label = ("Select Transformation"),
      choices = list(
        "None" = "none",
        "Log-10" = "log10",
        "Natural Log" = "log",
        "Percent/Z-score" = "zScore"
      ),
      selected = "none"
    )
  })

  output$resetUI <- renderUI({
    req(rvals$selectedData)
    rvals$selectedData
    tagList(
      actionButton("resetElements", "Reset elements to original", class = "mybtn"),
      actionButton("reset", "Reset all to original", class = "mybtn")
    )
  })

  observeEvent(input$resetElements,{
    rvals$selectedData[,rvals$chem] = rvals$importedData[,rvals$chem]
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
    transform.method = input$transform.method
    impute.method = input$impute.method

    if(isTRUE(inherits(rvals$unselectedData,"data.frame"))){
      if(impute.method == "none" & attr(rvals$selectedData,"impute.method") != "none"){
        impute.method = attr(rvals$selectedData,"impute.method")
      }
      if(transform.method == "none" & attr(rvals$selectedData,"transform.method") != "none"){
        transform.method = attr(rvals$selectedData,"transform.method")
      }

      rvals$selectedData = dplyr::bind_rows(rvals$selectedData,rvals$unselectedData) %>%
        dplyr::arrange(rowid) %>%
        dplyr::select(-tidyselect::all_of(input$chem)) %>%
        dplyr::left_join(rvals$importedData %>%
                           dplyr::select(rowid, tidyselect::all_of(input$chem)),by = "rowid")


    }
    rvals$selectedData =
      rvals$selectedData %>%
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

    # imputation
    if (impute.method  != "none") {
      quietly({
        rvals$selectedData[, rvals$chem] = mice::complete(mice::mice(rvals$selectedData[, rvals$chem], method = impute.method ))
      })
    }
    attr(rvals$selectedData,"impute.method") = impute.method
    showNotification("imputed data")

    # Transforming data
    quietly({
      suppressWarnings({
        if(transform.method != "none"){
          if (transform.method == 'zscale') {
            rvals$selectedData[, rvals$chem] = zScale(rvals$selectedData[, rvals$chem])
          } else if (transform.method %in% c("log10", "log")) {
            rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
              dplyr::mutate_all(transform.method) %>%
              dplyr::mutate_all(round, digits = 3)
          }
          showNotification('transformed data')
        } else {
          rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
            dplyr::mutate_all(round, digits = 3)
        }
      })
      # get rid of infinite values
      rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
        dplyr::mutate_all(list(function(c)
          dplyr::case_when(!is.finite(c) ~ 0, TRUE ~ c)))
    })
    attr(rvals$selectedData,"transform.method") = transform.method


    try({
      rvals$runPCA = input$runPCA
      rvals$runCDA = input$runCDA
      if(isTRUE(rvals$runPCA)) showNotification("Ran PCA")
      if(isTRUE(rvals$runCDA)) showNotification("Ran CDA")
    })
    showNotification("updated",duration = 3)
  })

  observeEvent(rvals$selectedData,{
    print("restoring state")
    restoreState(rvals = rvals,input = input,session = session)
  })

  output$newCol = renderUI({
    req(rvals$selectedData)
    actionButton('addNewCol', "Add New Column", class = "mybtn")
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
