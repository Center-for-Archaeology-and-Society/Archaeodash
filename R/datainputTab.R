#' UI elements for Data Input tab
#'
#' @return UI
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
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param rvals reactive values object
#'
#' @return server
#' @export
#'
#' @examples
#' dataInputServer(input, output, session, rvals)
dataInputServer = function(input, output, session, rvals) {

  observeEvent(input$file1, {
    req(input$file1)
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
      rvals$eligibleGroups = NULL
      rvals$sampleID = NULL
      rvals$impute.method = input$impute.method
      transform.method = input$transform.method
      # print(dput(input$file1))
      data = purrr::map_df(1:length(input$file1$datapath),function(i)rio::import(input$file1$datapath[i], setclass = 'tibble') %>% dplyr::mutate(file = tools::file_path_sans_ext(input$file1$name[i])) %>%
                             dplyr::mutate_all(as.character)) %>%
        setNames(janitor::make_clean_names(names(.),case = 'none')) %>%
        dplyr::select(-tidyselect::any_of('rowid')) %>%
        tibble::rowid_to_column() %>%
        dplyr::mutate_all("as.character")
      rvals$importedData = dplyr::bind_rows(rvals$importedData %>% dplyr::mutate_all(as.character),data)
      rvals$selectedData = dplyr::bind_rows(rvals$selectedData %>% dplyr::mutate_all(as.character),data)
    }
  })

  # Render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    print("attr")
    quietly(label = "attr",{
      df <- rvals$importedData
      dfNum = suppressWarnings(df %>% dplyr::mutate_all(as.numeric) %>%
                                 janitor::remove_empty("cols"))
      # Remove numeric columns from default selection
      nums1 <- names(dfNum)
      items = names(df[,which(!names(df) %in% nums1)])
      # hide columns with all unique values
      n = nrow(df)
      colLengths = vapply(df, function(x) length(unique(x)), integer(1))
      cols = which(colLengths < n)
      items2 = items[which(items %in% names(cols))]
      # Set names as all columns in datatable
      items.all <- names(df)
      names(items.all) = items.all
      names(items) = items
      names(items2) = items2
      if(isTRUE(is.null(rvals[['attr']])))
        selection = items else
          selection = rvals[['attr']]
      if(isTRUE(is.null(rvals[['attrGroups']])))
        selection2 = items2 else
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
  })

  # Render select lookup for choosing groups to include
  output$subSelect <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    req(input$attrGroups)
    print("subselect")
    quietly(label = "subselect",{
      df <- rvals$importedData
      items.all = quietly(label = 'items.all',df[[input$attrGroups]] %>% unique %>% sort)
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
  })

  # Render multi-select lookup for choosing chemical concentration columns
  output$chem <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    print("chem")
    df <- rvals$importedData
    dfNum = suppressWarnings(df %>% dplyr::mutate_all(as.numeric) %>%
                               janitor::remove_empty("cols"))
    # Remove numeric columns from default selection
    nums1 <- names(dfNum)
    quietly(label = "chem",{
      selected = nums1[which(nums1 != 'rowid')]
      choices = nums1[which(nums1 != 'rowid')]
      selectInput(
        "chem",
        "Select element concentrations:",
        choices,
        multiple = TRUE,
        selected = selected
      )
    })
  })

  # Render button to update datatable based on variable selections
  output$actionUI <- renderUI({
    req(input$file1)
    print("actionUI")
    quietly(label = "actionUI",{
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
  })

  # Render options for data imputation
  output$impute.options <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    print("impute.options")
    if(is.null(rvals$impute.method)){
      sel = "none"
    } else {
      sel = rvals$impute.method
    }
    quietly(label = "impute.options",{
      radioButtons(
        "impute.method",
        label = ("Select Imputation Method"),
        choices = list(
          "None" = "none",
          "Random Forest" = "rf",
          "Predictive Mean Matching" = "pmm",
          "Weighted Predictive Mean Matching" = "midastouch"
        ),
        selected = sel
      )
    })
  })

  # Render options for data transformation
  output$transform.options <- renderUI({
    req(req(nrow(rvals$selectedData) > 0))
    print("transform.options")
    if(is.null(rvals$transform.method)){
      sel = "none"
    } else {
      sel = rvals$transform.method
    }
    quietly(label = "transform.options",{
      radioButtons(
        "transform.method",
        label = ("Select Transformation"),
        choices = list(
          "None" = "none",
          "Log-10" = "log10",
          "Natural Log" = "log",
          "Percent/Z-score" = "zScore"
        ),
        selected = sel
      )
    })
  })

  output$resetUI <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    print("resetUI")
    quietly(label = "resetUI",{
      tagList(
        actionButton("resetElements", "Reset elements to original", class = "mybtn"),
        actionButton("reset", "Reset to last file import", class = "mybtn"),
        actionButton("resetClear", "Clear workspace", class = "mybtn"),

      )
    })
  })

  observeEvent(input$resetElements,{
    print("resetElements")
    rvals$selectedData[,rvals$chem] = rvals$importedData[,rvals$chem]
  })

  observeEvent(input$reset,{
    print("reset")
    showModal(modalDialog(title = "Confirm",shiny::p("Press to confirm. All changes will be lost"),footer = tagList(actionButton("confirmReset","confirm"),modalButton("cancel")),easyClose = T))
  })

  observeEvent(input$resetClear,{
    print("resetClear")
    showModal(modalDialog(title = "Confirm",shiny::p("Press to confirm. All data will be lost"),footer = tagList(actionButton("confirmResetClear","confirm"),modalButton("cancel")),easyClose = T))
  })

  observeEvent(input$confirmReset,{
    print("confirmReset")
    removeModal()
    rvals$chem = NULL
    rvals$attrGroups = NULL
    rvals$attr = NULL
    rvals$attrs = NULL
    rvals$attrGroupsSub = NULL
    rvals$selectedData = rvals$importedData
  })

  observeEvent(input$confirmResetClear,{
    print("confirmResetClear")
    removeModal()
    session$reload()
  })

  # create subset data frame
  observeEvent(input$action, {
    print("action")
    req(nrow(rvals$selectedData) > 0)
    req(input$attr)
    req(input$chem)
    rvals$chem = input$chem
    rvals$attrGroups = input$attrGroups
    rvals$attr = input$attr
    rvals$attrs = unique(c('rowid','file',rvals$attr,rvals$attrGroups))
    rvals$attrGroupsSub = input$attrGroupsSub
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
    rvals$transform.method = input$transform.method
    rvals$impute.method = input$impute.method
    transform.method = input$transform.method
    impute.method = input$impute.method

    message("subsetting data")
    print(rvals$chem)
    rvals$selectedData =
      quietly(label = "selectedData",{
        rvals$importedData %>%
          dplyr::select(
            rowid,
            file,
            tidyselect::any_of(input$attr),
            tidyselect::any_of(input$attrGroups),
            tidyselect::any_of(rvals$chem)
          ) %>%
          dplyr::mutate_at(dplyr::vars(input$attrGroups), factor) %>%
          dplyr::mutate_at(dplyr::vars(rvals$chem), quietly(as.numeric))
      })

    # imputation
    quietly(label = 'impute',{
      if (impute.method  != "none") {
        message("imputing")
        if(!is.null(rvals[[impute.method]]) && nrow(rvals[[impute.method]]) == nrow(rvals$importedData)){
          rvals$chem = rvals$chem[which(rvals$chem %in% colnames(rvals$selectedData))]
          rvals$selectedData[, rvals$chem] = rvals[[impute.method]]
        } else {
          transformed = rvals$importedData[, rvals$chem] %>%
            dplyr::mutate_all(dplyr::na_if,y = "0")
          rvals[[impute.method]] = tryCatch(mice::complete(mice::mice(transformed, method = impute.method)),error = function(e){
            mynotification(e)
            return(rvals$importedData[,rvals$chem])
          })
          if(!is.null(rvals[[impute.method]])){
            transformed = dplyr::bind_cols(rvals$importedData %>% dplyr::select(rowid), rvals[[impute.method]]) %>%
              dplyr::filter(rowid %in% rvals$selectedData$rowid)
            rvals$selectedData = dplyr::left_join(rvals$selectedData, transformed, by = 'rowid')
              missing = rvals$chem[which(!rvals$chem %in% names(rvals$selectedData))]
            if(length(missing) > 0){
              mynotification(paste("could not impute data for",missing))
              rvals$selectedData = dplyr::left_join(rvals$selectedData, rvals$importedData %>% dplyr::select(rowid,tidyselect::any_of(missing)), by = 'rowid') %>%
                dplyr::mutate_at(dplyr::vars(rvals$chem), quietly(as.numeric))
            }
            mynotification("imputed data")
          }
        }
      }
    })

    rvals$chem = rvals$chem[which(rvals$chem %in% colnames(rvals$selectedData))]

    # Transforming data
    quietly(label = "transform",{
      if(transform.method != "none"){
        message("transforming")
        suppressWarnings({
          if(!is.null(rvals[[transform.method]]) && nrow(rvals[[transform.method]]) == nrow(rvals$importedData)){
            rvals$selectedData[, rvals$chem ] = rvals[[transform.method]]
          } else {
            transformed = rvals$importedData[, rvals$chem ]  %>%
              dplyr::mutate_all(quietly(as.numeric))
            if (transform.method == 'zscale') {
              transformed = zScale(transformed)
            } else if (transform.method %in% c("log10", "log")) {
              transformed = transformed  %>%
                dplyr::mutate_all(transform.method) %>%
                dplyr::mutate_all(round, digits = 3)
            }
            # get rid of infinite values
            transformed = transformed %>%
              dplyr::mutate_all(list(function(c)
                dplyr::case_when(!is.finite(c) ~ 0, TRUE ~ c)))
            rvals$selectedData[, rvals$chem] = transformed
            mynotification('transformed data')
          }
        })
      }
    })

    if(isTRUE(is.null(input$attrGroupsSub))){
      mynotification("Cannot proceed without any groups selected",type = "error")
    } else {
      quietly(label = "keep",{rvals$selectedData = rvals$selectedData %>%
        dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub)})
    }

    try({
      rvals$runPCA = input$runPCA
      rvals$runCDA = input$runCDA
      if(isTRUE(rvals$runPCA)) mynotification("Ran PCA")
      if(isTRUE(rvals$runCDA)) mynotification("Ran CDA")
    })
    mynotification("updated",duration = 3)
  })

  observeEvent(rvals$selectedData,{
    req(nrow(rvals$selectedData) > 0)
    print("restoring state")
    restoreState(rvals = rvals,input = input,session = session)
  })

  output$newCol = renderUI({
    req(nrow(rvals$selectedData) > 0)
    actionButton('addNewCol', "Add New Column", class = "mybtn")
  })

  observeEvent(input$addNewCol, {
    print("adding new column")
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
    rvals$selectedData = quietly(label = "addNewCol",{
      rvals$selectedData %>%
        dplyr::select(-tidyselect::any_of(newCol)) %>%
        dplyr::mutate(!!as.name(newCol) := factor(val))
    })
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  })

}
