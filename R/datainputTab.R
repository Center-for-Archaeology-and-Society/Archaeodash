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
    uiOutput('newCol'),
    br(),
    hr(),
    uiOutput("attr"),
    uiOutput("subSelect"),
    uiOutput("chemUI"),
    uiOutput("actionUI"),
    hr(),
    uiOutput("resetUI"),
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
  pending_new_column <- shiny::reactiveVal(NULL)
  available_group_values <- shiny::reactiveVal(character())
  active_group_column <- shiny::reactiveVal(NULL)

  update_group_selector <- function(selected_values) {
    selected_values <- as.character(selected_values)
    updateSelectizeInput(session, "attrGroupsSub", selected = selected_values)
  }

  apply_new_column <- function(new_column_name, new_value) {
    rvals$importedData = quietly(label = "addNewCol",{
      rvals$importedData %>%
        dplyr::select(-tidyselect::any_of(new_column_name)) %>%
        dplyr::mutate(!!as.name(new_column_name) := factor(as.character(new_value)))
    })
    rvals$selectedData = quietly(label = "addNewCol2",{
      rvals$selectedData %>%
        dplyr::select(-tidyselect::any_of(new_column_name)) %>%
        dplyr::mutate(!!as.name(new_column_name) := factor(as.character(new_value)))
    })
    if (is.null(rvals$attr)) {
      rvals$attr <- names(rvals$importedData)
    } else {
      rvals$attr <- unique(c(rvals$attr, new_column_name))
    }
    if (!is.null(rvals$attrs)) {
      rvals$attrs <- unique(c(rvals$attrs, new_column_name))
    }
    updateCurrent(rvals,con,credentials,input,output,session)
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  }


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
      rvals$initialChem = tblsmd[tblsmd %in% names(rvals$importedData)]
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
      dataLoaderUI()

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
      items.all <- as.character(items.all)
      items.all <- items.all[!is.na(items.all)]
      available_group_values(items.all)
      group_counts <- df %>%
        dplyr::mutate(.group_value = as.character(.data[[input$attrGroups]])) %>%
        dplyr::filter(!is.na(.group_value)) %>%
        dplyr::count(.group_value, name = ".n", sort = FALSE)
      group_mode <- if (is.null(input$groupSelectionMode)) "all" else input$groupSelectionMode
      prior_selection <- if (!is.null(rvals[["attrGroupsSub"]])) {
        rvals[["attrGroupsSub"]]
      } else {
        isolate(input$attrGroupsSub)
      }
      prior_group_column <- active_group_column()
      if (is.null(prior_group_column)) {
        prior_group_column <- input$attrGroups
      }
      selection <- resolve_group_selection(
        all_groups = items.all,
        prior_selection = prior_selection,
        prior_group_column = prior_group_column,
        current_group_column = input$attrGroups
      )
      display_items <- filter_group_choices(
        all_groups = items.all,
        selection = selection,
        mode = group_mode
      )
      display_counts <- group_counts[group_counts$.group_value %in% display_items, , drop = FALSE]
      if (nrow(display_counts) > 0) {
        display_values <- as.character(display_counts$.group_value)
        display_labels <- paste0(display_values, " (n=", as.integer(display_counts$.n), ")")
        display_choices <- display_values
        names(display_choices) <- display_labels
      } else {
        display_choices <- character()
      }
      picker_selected <- if (group_mode == "unselected") character() else selection
      tagList(
        radioButtons(
          "groupSelectionMode",
          "View groups",
          choices = c("All groups" = "all", "Only selected" = "selected", "Only unselected" = "unselected"),
          selected = group_mode,
          inline = TRUE
        ),
        tags$div(
          style = "display:grid; grid-template-columns:1fr 1fr; gap:0.2rem; margin:0.15rem 0 0.35rem 0;",
          actionButton("groupSelectAll", "Select all", class = "mybtn", style = "margin:0; padding:2px 6px; width:100%;"),
          actionButton("groupDeselectAll", "Deselect all", class = "mybtn", style = "margin:0; padding:2px 6px; width:100%;"),
          actionButton("groupInvertSelection", "Invert selection", class = "mybtn", style = "margin:0; padding:2px 6px; width:100%;"),
          actionButton("groupResetSelection", "Reset to all", class = "mybtn", style = "margin:0; padding:2px 6px; width:100%;")
        ),
        selectizeInput(
          "attrGroupsSub",
          "Select groups to include",
          choices = display_choices,
          multiple = TRUE,
          selected = picker_selected,
          options = list(plugins = list("remove_button"))
        )
      )
    })
  })

  observeEvent(input$attrGroupsSub, {
    if (is.null(input$attrGroupsSub)) return(NULL)
    rvals$attrGroupsSub <- input$attrGroupsSub
  }, ignoreNULL = FALSE)

  observeEvent(input$attrGroups, {
    if (is.null(input$attrGroups)) return(NULL)
    previous_group_column <- active_group_column()
    if (!is.null(previous_group_column) && !identical(previous_group_column, input$attrGroups)) {
      # Reset subgroup selection when switching to a different grouping column.
      rvals$attrGroupsSub <- NULL
    }
    active_group_column(input$attrGroups)
  }, ignoreNULL = FALSE)

  observeEvent(input$groupSelectionMode, {
    if (is.null(input$groupSelectionMode)) return(NULL)
    selected_values <- if (is.null(input$attrGroupsSub)) character() else input$attrGroupsSub
    if (input$groupSelectionMode == "selected" && length(selected_values) == 0) {
      mynotification("No groups are currently selected.", type = "warning")
    }
    if (input$groupSelectionMode == "unselected" && length(setdiff(available_group_values(), selected_values)) == 0) {
      mynotification("All groups are currently selected.", type = "warning")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$groupSelectAll, {
    selected_values <- available_group_values()
    rvals$attrGroupsSub <- selected_values
    update_group_selector(selected_values)
  })

  observeEvent(input$groupDeselectAll, {
    rvals$attrGroupsSub <- character()
    update_group_selector(character())
  })

  observeEvent(input$groupInvertSelection, {
    selected_values <- if (is.null(input$attrGroupsSub)) character() else input$attrGroupsSub
    updated_values <- setdiff(available_group_values(), selected_values)
    rvals$attrGroupsSub <- updated_values
    update_group_selector(updated_values)
  })

  observeEvent(input$groupResetSelection, {
    selected_values <- available_group_values()
    rvals$attrGroupsSub <- selected_values
    update_group_selector(selected_values)
    if (!is.null(input$groupSelectionMode) && input$groupSelectionMode != "all") {
      updateRadioButtons(session, "groupSelectionMode", selected = "all")
    }
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
        checkboxInput("runPCA","check to run PCA", value = TRUE),
        checkboxInput("runUMAP","check to run UMAP", value = TRUE),
        checkboxInput("runLDA","check to run LDA", value = TRUE),
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
                 dplyr::mutate_at(dplyr::vars(rvals$chem), quietly(as.numeric)),
               error = function(e) mynotification(e))

    if(isTRUE(rvals$loadNAAsZero)){
      rvals$selectedData = rvals$selectedData %>%
        dplyr::mutate_at(dplyr::vars(rvals$chem), tidyr::replace_na, 0)
    }
    message("data subsetted")

    # imputation
    quietly(label = 'impute',{
      if (rvals$impute.method  != "none" & !is.null(rvals$impute.method) & nrow(rvals$selectedData) > 0) {
        if (!app_require_packages("mice", feature = "Imputation")) {
          return(NULL)
        }
        message("imputing")
        transformed = rvals$selectedData[, rvals$chem] %>%
          dplyr::mutate_all(quietly(as.numeric))
        if(isTRUE(rvals$loadZeroAsNA)){
          transformed = transformed %>%
            dplyr::mutate_all(dplyr::na_if, y = 0)
        }
        if(isTRUE(rvals$loadNegativeAsNA)){
          transformed = transformed %>%
            dplyr::mutate_all(~ dplyr::if_else(. < 0, NA_real_, .))
        }
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
    print("adding column")
    if(isTRUE(is.null(input$createGroup))) newCol = "cluster" else newCol = input$createGroup
    if(isTRUE(is.null(input$createGroupVal))) val = "1" else val = input$createGroupVal
    removeModal()
    if(newCol %in% names(rvals$importedData) || newCol %in% names(rvals$selectedData)){
      pending_new_column(list(name = newCol, value = val))
      showModal(modalDialog(
        title = "Overwrite existing column?",
        p(paste0("Column '", newCol, "' already exists and will be overwritten.")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmOverwriteNewCol", "Overwrite")
        ),
        easyClose = TRUE
      ))
    } else {
      apply_new_column(new_column_name = newCol, new_value = val)
    }
  })

  observeEvent(input$confirmOverwriteNewCol, {
    req(pending_new_column())
    pending = pending_new_column()
    removeModal()
    apply_new_column(new_column_name = pending$name, new_value = pending$value)
    pending_new_column(NULL)
  })

}
