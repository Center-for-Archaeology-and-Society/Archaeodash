#' UI elements for Data Input tab
#'
#' @return UI
#' @export
#'
#' @examples
#' datainputTab()
datainputTab = function() {
  tagList(
    h3("Upload Data"),
    fileInput("file1", "Choose File (csv, xlsx or other supported format)",multiple = F),
    uiOutput('priorDatasets'),
    uiOutput('confirmPriorUI'),
    br(),
    uiOutput('manageDatasets'),
    hr(),
    uiOutput("attr"),
    uiOutput("subSelect"),
    uiOutput("chemUI"),
    uiOutput("actionUI"),
    hr(),
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
dataInputServer = function(input, output, session, rvals, con, credentials) {

  observe({
    if(is.null(con)){
      shiny::invalidateLater(1000, session)
      rvals$tbls = NULL
    } else {
    tbls = DBI::dbListTables(con)
    tbls = tbls[which(stringr::str_detect(tbls,paste0("^",credentials$res$username,"_")))]
    tbls = tbls[which(!stringr::str_detect(tbls,"_metadata"))]
    rvals$tbls = tbls
    }
  })

  output$confirmPriorUI = renderUI({
    if(isTruthy(credentials$status)){
      actionButton("confirmPrior","Confirm dataset selection", class = 'mybtn')
    } else {
      NULL
    }
  })

  output$priorDatasets = renderUI({
    if(isTruthy(credentials$status)){
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

  observeEvent(input$confirmPrior,{
    req(input$selectedDatasets)

    null_vars <- c("chem", "attrGroups", "attr", "attrs", "attrGroupsSub",
                   "xvar", "xvar2", "yvar", "yvar2", "data.src", "Conf",
                   "int.set", "eligibleGroups", "sampleID")

    # Loop through the list and set each to NULL
    for (var in null_vars) {
      rvals[[var]] <- NULL
    }

    tbls = list()
    for(tbl in input$selectedDatasets){
      tbls[[tbl]] = dplyr::tbl(con,tbl) %>% dplyr::collect() %>%
        dplyr::mutate_all(as.character)
    }
    rvals$importedData = do.call(dplyr::bind_rows,tbls) %>%
      dplyr::select(-tidyselect::any_of('rowid')) %>%
      dplyr::distinct_all() %>%
      tibble::rowid_to_column()
    rvals$selectedData = rvals$importedData

    filenames = paste0(input$selectedDatasets,"_metadata")

    # get metadata
    tblsmd = list()
    for(tbl in filenames){
      if(DBI::dbExistsTable(con,tbl)){
        tblsmd[[tbl]] = dplyr::tbl(con,tbl) %>% dplyr::collect() %>%
          dplyr::mutate_all(as.character)
      }
    }
    if(length(tblsmd) > 0){
      tblsmd = do.call(dplyr::bind_rows,tblsmd) %>%
        dplyr::distinct_all() %>%
        dplyr::filter(field == "variable") %>%
        dplyr::pull(value)
      rvals$chem = tblsmd
    }

  })

  output$manageDatasets = renderUI({
    if(isTruthy(credentials$status)){
      tagList(
        h3("Manage datasets"),
        actionButton("deleteDatasets","Delete selected dataset(s)", class = 'mybtn'),
        actionButton("mergeDatasets","Merge/rename selected datasets", class = 'mybtn')
      )
    } else {
      NULL
    }
  })

  observeEvent(input$deleteDatasets,{
    req(input$selectedDatasets)
    showModal(modalDialog(
      title = "Delete selected datasets",
      "Are you sure you want to delete the selected datasets?",
      footer = tagList(
        actionButton("deleteDatasetsconfirm","Yes"),
        modalButton("No")
      )
    ))
  })

  observeEvent(input$deleteDatasetsconfirm,{
    removeModal()
    req(input$selectedDatasets)
    print('deleting datasets')
    for(tbl in input$selectedDatasets){
      DBI::dbRemoveTable(con,tbl)
      DBI::dbRemoveTable(con,paste0(tbl,"_metadata"))
    }
  })

  observeEvent(input$mergeDatasets,{
    req(input$selectedDatasets)
    showModal(modalDialog(
      title = "Merge selected datasets",
      "Are you sure you want to merge or rename the selected datasets?",
      textInput("mergeName","Enter new dataset name (do not include login prefix, e.g.(not 'username_newdataset' but 'newdataset'))"),
      footer = tagList(
        actionButton("mergeDatasetsconfirm","Yes"),
        modalButton("No")
      )
    ))
  })

  observeEvent(input$mergeDatasetsconfirm,{
    removeModal()
    req(input$selectedDatasets)
    req(input$mergeName %>% length() > 0)
    print("merging datasets")
    tryCatch({
      tbls = list()
      for(tbl in input$selectedDatasets){
        tbls[[tbl]] = dplyr::tbl(con,tbl) %>% dplyr::collect() %>%
          dplyr::mutate_all(as.character)
      }
      merged = do.call(dplyr::bind_rows,tbls) %>%
        dplyr::select(-tidyselect::any_of('rowid')) %>%
        dplyr::distinct_all() %>%
        tibble::rowid_to_column()

      filename = paste0(credentials$res$username,"_",input$mergeName)

      # get metadata
      tblsmd = list()
      for(tbl in input$selectedDatasets){
        tblnm = paste0(tbl,"_metadata")
        dblist = DBI::dbListTables(con)
        if(tblnm %in% dblist){
          tblsmd[[tblnm]] = dplyr::tbl(con,tblnm) %>% dplyr::collect() %>%
            dplyr::mutate_all(as.character)
        }
      }
      if(length(tblsmd) > 0){
        tblsmd = do.call(dplyr::bind_rows,tblsmd) %>%
          dplyr::distinct_all() %>%
          dplyr::filter(!field %in% c("created","dataset")) %>%
          dplyr::bind_rows(tibble::tibble(field = "dataset",value = filename),
                           tibble::tibble(field = "created",value = as.character(as.Date(Sys.time())))
          )
      } else {
        tblsmd = tibble::tibble(field = "datasetName",value = filename,
                                field = "created",value = as.character(as.Date(Sys.time())))
      }

      if(!DBI::dbExistsTable(con,filename)){
        DBI::dbWriteTable(con,filename,merged, row.names = F)
        DBI::dbWriteTable(con,paste0(filename,"_metadata"),tblsmd, row.names = F)
      } else {
        rvals$mergeFilename = filename
        rvals$merged = merged
        rvals$merged_metadata = tblsmd
        showModal(modalDialog(
          title = "confirm?",
          p("Dataset name already exists. Do you want to overwrite this table?"),
          footer = tagList(
            actionButton("overwriteDataset","Yes"),
            modalButton("No")
          )
        ))
      }
    }, error = function(e){
      mynotification(paste("Error merging datasets\n",e), type = "error")
    })
  })

  observeEvent(input$overwriteDataset,{
    removeModal()
    req(rvals$mergeFilename)
    req(rvals$merged)
    req(rvals$merged_metadata)
    DBI::dbWriteTable(conn = con,name = rvals$mergeFilename,value = rvals$merged, row.names = F, overwrite = T)
    DBI::dbWriteTable(conn = con,name = paste0(rvals$mergeFilename,"_metadata"),value = rvals$merged_metadata, row.names = F, overwrite = T)
    mynotification("merged datasets")
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

      # print(dput(input$file1))

      rvals$data = dataLoader(filename = input$file1$datapath)
      if(isTruthy(credentials$status)){
        dataLoaderUI()
      } else {
        rvals$importedData = rvals$data
        rvals$selectedData = rvals$importedData
      }

    }
  })

  dataLoaderServer(rvals = rvals,input,output,session, credentials = credentials, con = con)

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
      items.all = quietly(label = 'items.all',df %>%
                            dplyr::select(tidyselect::any_of(input$attrGroups)) %>% dplyr::pull() %>% unique %>% sort)
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
  output$chemUI <- renderUI({
    req(nrow(rvals$importedData) > 0)
    print("chem")
    print("existing rvals$chem:")
    print(rvals$chem)
    df <- rvals$importedData
    dfNum = suppressWarnings(df %>% dplyr::mutate_all(as.numeric) %>%
                               janitor::remove_empty("cols"))
    # Remove non-numeric columns from default selection
    nums1 <- names(dfNum)
    quietly(label = "chem",{
      all = nums1[which(nums1 != 'rowid')]
      if(is.null(rvals$chem)){
        selected = all
      } else {
        selected = rvals$chem
      }
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
        checkboxInput("runUMAP","check to run UMAP", value = F),
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
        # actionButton("saveTbl", "Save data to new table", class = "mybtn"),
        actionButton("resetClear", "Clear workspace", class = "mybtn"),

      )
    })
  })

  observeEvent(input$resetElements,{
    print("resetElements")
    rvals$selectedData[,rvals$chem] = rvals$importedData[,rvals$chem]
  })

  # observeEvent(input$saveTbl,{
  #   print("saveTbl")
  #   if(isTruthy(credentials$status)){
  #     newtbl = rvals$selectedData %>%
  #       dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub) %>%
  #       dplyr::select(-tidyselect::any_of(input$chem))
  #     newtblchems = rvals$importedData %>%
  #       dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub) %>%
  #       dplyr::select(tidyselect::any_of(input$chem))
  #     newtbl = dplyr::bind_cols(newtbl,newtblchems)
  #     rvals$importedData = newtbl
  #     dataLoaderUI()
  #   } else {
  #     mynotification("Must be logged in to save table", type = "warning")
  #   }
  # })

  observeEvent(input$resetClear,{
    print("resetClear")
    showModal(modalDialog(title = "Confirm",shiny::p("Press to confirm. All data will be lost"),footer = tagList(actionButton("confirmResetClear","confirm"),modalButton("cancel")),easyClose = T))
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
    rvals$runPCA = input$runPCA
    rvals$runLDA = input$runLDA
    rvals$runUMAP = input$runUMAP
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
                 dplyr::mutate_at(dplyr::vars(rvals$chem), quietly(as.numeric)) %>%
                 dplyr::mutate_at(dplyr::vars(rvals$chem),tidyr::replace_na,0),error = function(e) mynotification(e))
    message("data subsetted")

    # imputation
    quietly(label = 'impute',{
      if (rvals$impute.method  != "none" & !is.null(rvals$impute.method) & nrow(rvals$selectedData) > 0) {
        message("imputing")
        transformed = rvals$selectedData[, rvals$chem] %>%
          dplyr::mutate_all(dplyr::na_if,y = 0)
        transformed = tryCatch(mice::complete(mice::mice(transformed, method = rvals$impute.method)),
                               error = function(e){
          mynotification(e)
          return(rvals$selectedData[,rvals$chem])
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
          transformed = rvals$selectedData[, rvals$chem ]  %>%
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
    rvals$selectedData = quietly(rvals$selectedData %>%
      dplyr::mutate_at(dplyr::vars(input$attrGroups), as.character) %>%
      dplyr::mutate_at(dplyr::vars(input$attrGroups),factor))

    try({
      rvals$runPCAx = input$runPCA
      rvals$runLDAx = input$runLDA
      rvals$runUMAPx = input$runUMAP
      # if(isTRUE(rvals$runPCA)) mynotification("Ran PCA")
      # if(isTRUE(rvals$runUMAP)) mynotification("Ran UMAP")
      # if(isTRUE(rvals$runLDA)) mynotification("Ran LDA")
    })

    # update current table
    updateCurrent(rvals,
                  con,
                  credentials,
                  input,
                  output,
                  session)
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
    rvals$selectedData = quietly(label = "addNewCol2",{
      rvals$selectedData %>%
        dplyr::select(-tidyselect::any_of(newCol)) %>%
        dplyr::mutate(!!as.name(newCol) := factor(as.character(val)))
    })
    updateCurrent(rvals,con,credentials,input,output,session)
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  })

}
