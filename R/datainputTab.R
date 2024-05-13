#' UI elements for Data Input tab
#'
#' @return UI
#' @export
#'
#' @examples
#' datainputTab()
datainputTab = function() {
  tagList(
    h3("Choose previously loaded data or upload a new dataset (upload one at a time)"),
    uiOutput('priorDatasets'),
    fileInput("file1", "Choose File (csv, xlsx or other supported format)",multiple = F),
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
dataInputServer = function(input, output, session, rvals, con) {

  observe({
    shiny::invalidateLater(1000, session)
    tbls = DBI::dbListTables(con)
    rvals$tbls = tbls[which(stringr::str_detect(tbls,paste0("^",credentials$res$username,"_")))]
  })

  output$priorDatasets = renderUI({
    if(!is.null(credentials$res$username) && !is.na(credentials$res$username)){
      if(any(stringr::str_detect(rvals$tbls,"_current"))){
        selection = rvals$tbls[which(stringr::str_detect(rvals$tbls,"_current"))]
      } else {
        selection = rvals$tbls[1]
      }
      selectInput("selectedDatasets", "Choose dataset(s)", choices = rvals$tbls, multiple = T, selected = selection)
    } else {
      NULL
    }
  })

  observeEvent(input$selectedDatasets,{
    req(input$selectedDatasets)
    print("selectedDatasets")
    tbls = list()
    for(tbl in input$selectedDatasets){
      tbls[[tbl]] = dplyr::tbl(con,tbl) %>% dplyr::collect() %>%
        dplyr::mutate_all(as.character)
    }
    rvals$importedData = do.call(dplyr::bind_rows,tbls) %>%
      dplyr::select(-tidyselect::any_of('rowid')) %>%
      tibble::rowid_to_column()
    rvals$selectedData = rvals$importedData
  })

  observeEvent(input$file1, {
    req(input$file1)
    print("importing file")
    if (!is.null(input$file1)) {
      null_vars <- c("chem", "attrGroups", "attr", "attrs", "attrGroupsSub",
                     "xvar", "xvar2", "yvar", "yvar2", "data.src", "Conf",
                     "int.set", "eligibleGroups", "sampleID")

      # Loop through the list and set each to NULL
      for (var in null_vars) {
        rvals[[var]] <- NULL
      }

      rvals$impute.method = input$impute.method
      rvals$transform.method = input$transform.method
      # print(dput(input$file1))

      rvals$importedData = dataLoader(filename = input$file1$datapath)
      if(!is.null(credentials$res$username) && !is.na(credentials$res$username)){
        dataLoaderUI()
      } else {
        rvals$selectedData = rvals$importedData
      }

    }
  })

  dataLoaderServer(data = rvals$importedData,input,output,session, credentials = credentials, con = con)

  # Render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    req(nrow(rvals$importedData) > 0)
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
      # if(isTRUE(is.null(rvals[['attr']])))
      #   selection = items else
      #     selection = rvals[['attr']]
      selection = names(rvals$importedData)
      if(isTRUE(is.null(rvals[['attrGroups']])))
        selection2 = items2 else
          selection2 = rvals[['attrGroups']]
      tagList(
        shinyjs::hidden(selectInput(
          "attr",
          "Select attribute variables you want to display:",
          items.all,
          multiple = TRUE,
          selected = selection
        )),
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
    req(nrow(rvals$importedData) > 0)
    req(input$attrGroups)
    print("subselect")
    quietly(label = "subselect",{
      df <- rvals$importedData
      items.all = quietly(label = 'items.all',df %>% dplyr::select(tidyselect::any_of(input$attrGroups)) %>% dplyr::pull() %>% unique %>% sort)
      print("subSelect")
      if(isTRUE(is.null(rvals[['attrGroupsSub']])))
        selection = items.all else
          selection = rvals[['attrGroupsSub']]
      if(!selection[1] %in% items.all) selection = items.all
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
    req(nrow(rvals$importedData) > 0)
    print("chem")
    df <- rvals$importedData
    dfNum = suppressWarnings(df %>% dplyr::mutate_all(as.numeric) %>%
                               janitor::remove_empty("cols"))
    # Remove non-numeric columns from default selection
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
    req(rvals$importedData)
    print("actionUI")
    quietly(label = "actionUI",{
      tagList(
        uiOutput("impute.options"),
        br(),
        uiOutput("transform.options"),
        br(),
        checkboxInput("runPCA","check to run PCA", value = F),
        checkboxInput("runLDA","check to run LDA", value = F),
        actionButton("action", "Press to confirm selections", class = "mybtn")
      )
    })
  })

  # Render options for data imputation
  output$impute.options <- renderUI({
    req(nrow(rvals$importedData) > 0)
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
    req(req(nrow(rvals$importedData) > 0))
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
    req(nrow(rvals$importedData) > 0)
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
    req(nrow(rvals$importedData) > 0)
    req(input$attr)
    req(input$chem)
    rvals$chem = input$chem
    rvals$attrGroups = input$attrGroups
    rvals$attr = input$attr
    rvals$attrs = unique(c(rvals$attr,rvals$attrGroups,"imputation","transformation"))
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

    message("subsetting data")

    # print(rvals$chem)
    # print(rvals$importedData)
    # print(input$attrGroups)
    # print(input$attr)

    rvals$selectedData =
        tryCatch(rvals$importedData %>%
          dplyr::select(
            tidyselect::any_of(rvals$attrGroups),
            tidyselect::any_of(rvals$chem),
            tidyselect::any_of(rvals$attr)
          ) %>%
          dplyr::mutate_at(dplyr::vars(rvals$attrGroups), factor) %>%
          dplyr::mutate_at(dplyr::vars(rvals$chem), quietly(as.numeric)),error = function(e) mynotification(e))
    message("data subsetted")

    # imputation
    quietly(label = 'impute',{
      if (rvals$impute.method  != "none" & !is.null(rvals$impute.method) & nrow(rvals$importedData) > 0) {
        message("imputing")
        transformed = rvals$importedData[, rvals$chem] %>%
          dplyr::mutate_all(dplyr::na_if,y = "0")
        transformed = tryCatch(mice::complete(mice::mice(transformed, method = rvals$impute.method)),error = function(e){
          mynotification(e)
          return(rvals$importedData[,rvals$chem])
        })
        if(is.data.frame(transformed)){
          rvals$selectedData[,rvals$chem] = transformed
          rvals$selectedData = rvals$selectedData %>%
            dplyr::mutate(imputation = rvals$impute.method)
        } else {
          mynotification("imputation failed")
        }
        mynotification("imputed data")
      }
    })

    rvals$chem = rvals$chem[which(rvals$chem %in% colnames(rvals$selectedData))]

    # Transforming data
    quietly(label = "transform",{
      if(rvals$transform.method != "none"){
        message("transforming")
        suppressWarnings({
          transformed = rvals$importedData[, rvals$chem ]  %>%
            dplyr::mutate_all(quietly(as.numeric))
          if (rvals$transform.method == 'zscale') {
            transformed = zScale(transformed)
          } else if (rvals$transform.method %in% c("log10", "log")) {
            transformed = transformed  %>%
              dplyr::mutate_all(rvals$transform.method) %>%
              dplyr::mutate_all(round, digits = 3)
          }
          # get rid of infinite values
          transformed = transformed %>%
            dplyr::mutate_all(list(function(c)
              dplyr::case_when(!is.finite(c) ~ 0, TRUE ~ c)))
          rvals$selectedData[, rvals$chem] = transformed
          rvals$selectedData = rvals$selectedData %>%
            dplyr::mutate(transformation = rvals$transform.method)
          mynotification('transformed data')
        })
    }
    })

    if(isTRUE(is.null(input$attrGroupsSub))){
      mynotification("Cannot proceed without any groups selected",type = "error")
    } else {
      message("filtering")
      quietly(label = "keep",{rvals$selectedData = rvals$selectedData %>%
        dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub)})
    }

    message("dropping factor levels")
    rvals$selectedData = rvals$selectedData %>%
      dplyr::mutate_at(dplyr::vars(input$attrGroups), droplevels)

    try({
      rvals$runPCA = input$runPCA
      rvals$runLDA = input$runLDA
      if(isTRUE(rvals$runPCA)) mynotification("Ran PCA")
      if(isTRUE(rvals$runLDA)) mynotification("Ran LDA")
    })

    # save current table to database
    if(!is.null(credentials$res$username) && !is.na(credentials$res$username)){
      message("saving data to database")
      DBI::dbWriteTable(con,paste0(credentials$res$username,"_current"),rvals$selectedData,overwrite = TRUE, row.names = F)
    }
    mynotification("updated",duration = 3)
    })

  observeEvent(rvals$selectedData,{
    req(nrow(rvals$importedData) > 0)
    print("restoring state")
    restoreState(rvals = rvals,input = input,session = session)
  })

  output$newCol = renderUI({
    req(nrow(rvals$importedData) > 0)
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
    rvals$importedData = quietly(label = "addNewCol",{
      rvals$importedData %>%
        dplyr::select(-tidyselect::any_of(newCol)) %>%
        dplyr::mutate(!!as.name(newCol) := factor(as.character(val)))
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
