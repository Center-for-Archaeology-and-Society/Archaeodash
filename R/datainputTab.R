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
      rvals$unselectedData = NULL
      rvals$eligibleGroups = NULL
      rvals$sampleID = NULL
      rvals$numCols = NULL
      # print(dput(input$file1))
      data = purrr::map_df(1:length(input$file1$datapath),function(i)rio::import(input$file1$datapath[i], setclass = 'tibble') %>% dplyr::mutate(file = tools::file_path_sans_ext(input$file1$name[i])) %>%
                             dplyr::mutate_all(as.character)) %>%
        setNames(janitor::make_clean_names(names(.),case = 'none')) %>%
        dplyr::select(-tidyselect::any_of('rowid')) %>%
        tibble::rowid_to_column()
      rvals$importedData = dplyr::bind_rows(rvals$importedData,data)
      rvals$selectedData = dplyr::bind_rows(rvals$selectedData,data)

    }
  })

  # Render multi-select lookup for choosing attribute columns
  output$attr <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    print("attr")
    quietly(label = "attr",{
      df <- dplyr::bind_rows(rvals$selectedData,rvals$unselectedData)
      dfNum = suppressWarnings(df %>% dplyr::mutate_all(as.numeric) %>%
                                 janitor::remove_empty("cols"))
      # Remove numeric columns from default selection
      nums1 <- names(dfNum)
      rvals$numCols = nums1
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
      df <- dplyr::bind_rows(rvals$selectedData,rvals$unselectedData)
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
    quietly(label = "chem",{
      selected = rvals$numCols[which(rvals$numCols != 'rowid')]
      selectInput(
        "chem",
        "Select element concentrations:",
        rvals$numCols,
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
        checkboxInput("runCDA","check to run CDA", value = F),
        actionButton("action", "Press to confirm selections", class = "mybtn")
      )
    })
  })

  # Render options for data imputation
  output$impute.options <- renderUI({
    req(nrow(rvals$selectedData) > 0)
    print("impute.options")
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
        selected = "none"
      )
    })
  })

  # Render options for data transformation
  output$transform.options <- renderUI({
    req(req(nrow(rvals$selectedData) > 0))
    print("transform.options")
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
        selected = "none"
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

    quietly(label = "combine prior data",{
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
    })
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
      mynotification("Cannot proceed without any groups selected",type = "error")
    } else {
      keep = quietly(label = "keep",rvals$selectedData %>%
                       dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub))
      discard = quietly(label = "discard",rvals$selectedData %>%
                          dplyr::filter(!rowid %in% keep$rowid))
      rvals$selectedData = keep
      rvals$unselectedData = discard
    }

    # imputation
    if (impute.method  != "none") {
      quietly(label = "impute",{
        rvals$selectedData[, rvals$chem] = mice::complete(mice::mice(rvals$selectedData[, rvals$chem], method = impute.method ))
      })
    }
    attr(rvals$selectedData,"impute.method") = impute.method
    mynotification("imputed data")

    # Transforming data
    quietly(label = "transform",{
      suppressWarnings({
        if(transform.method != "none"){
          if (transform.method == 'zscale') {
            rvals$selectedData[, rvals$chem] = zScale(rvals$selectedData[, rvals$chem])
          } else if (transform.method %in% c("log10", "log")) {
            rvals$selectedData[, rvals$chem] = rvals$selectedData[, rvals$chem] %>%
              dplyr::mutate_all(transform.method) %>%
              dplyr::mutate_all(round, digits = 3)
          }
          mynotification('transformed data')
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
    print("rendering new column")
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
