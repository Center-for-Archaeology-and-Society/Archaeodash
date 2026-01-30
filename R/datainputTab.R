#' UI elements for Data Input tab
#'
#' @return UI
#' @export
#'
#' @examples
#' datainputTab()
datainputTab <- function() {
  tagList(
    h3("Upload Data"),
    fluidRow(
      column(
        10,
        fileInput("file1", "Choose File (csv, xlsx or other supported format)", multiple = F)
      ),
      column(
        2,
        help_icon("", "Data must include at minimum the numeric variables used for analysis (i.e., elemental concentrations)."),
      )
    ),
    uiOutput("manageDatasets"),
    uiOutput("datasetChoice"),
    uiOutput("anidUI"),
    uiOutput("subSelect"),
    uiOutput("chemUI"),
    uiOutput("actionUI"),
    uiOutput("newCol"),
    uiOutput("resetUI")
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
dataInputServer <- function(input, output, session, rvals, con, credentials) {
  observeEvent(input$file1, {
    req(input$file1)
    print("importing file")
    if (!is.null(input$file1)) {
      # null_vars <- c(
      #   "chem", "attrGroups", "attr", "attrs", "attrGroupsSub",
      #   "xvar", "xvar2", "yvar", "yvar2", "data.src", "Conf",
      #   "int.set", "eligibleGroups", "sampleID"
      # )

      # # Loop through the list and set each to NULL
      # for (var in null_vars) {
      #   rvals[[var]] <- NULL
      # }

      # print(dput(input$file1))

      rvals$data <- dataLoader(filename = input$file1$datapath)
      if (isTruthy(credentials$status)) {
        dataLoaderUI()
      } else {
        rvals$importedData <- rvals$data
        rvals$datasets$original <- rvals$importedData
      }
    }
  })

  dataLoaderServer(rvals = rvals, input, output, session, credentials = credentials, con = con)

  output$datasetChoice <- renderUI({
    req(nrow(rvals$importedData) > 0)
    tagList(
      h3("Choose dataset"),
      selectInput(
        "selectedData",
        "Select dataset",
        choices = names(rvals$datasets),
        selected = rvals$selectedData
      )
    )
  })

  # Render multi-select lookup for choosing attribute columns
  output$anidUI <- renderUI({
    req(nrow(rvals$datasets[[rvals$selectedData]]) > 0)
    # print("attr")
    quietly(label = "attr", {
      df <- rvals$datasets[[rvals$selectedData]]
      dfNum <- suppressWarnings(df %>% dplyr::mutate_all(as.numeric) %>%
        janitor::remove_empty("cols"))
      rvals$dfNum <- dfNum
      # Remove numeric columns from default selection
      nums1 <- names(dfNum)
      items <- names(df[, which(!names(df) %in% nums1)])
      # hide columns with all identical values
      n <- nrow(df)
      colLengths <- vapply(df, function(x) length(unique(x)), integer(1))
      cols <- which(colLengths < n)
      items2 <- items[which(items %in% names(cols))]
      # Set names as all columns in datatable
      items.all <- names(df)
      names(items.all) <- items.all
      names(items) <- items
      names(items2) <- items2
      choices <- names(df)
      choiceLengths <- sapply(choices, function(x) length(unique(df[[x]])))
      choices <- choices[which(choiceLengths == nrow(df))]
      if ("anid" %in% tolower(choices)) {
        selected <- choices[which(tolower(choices) == "anid")]
      } else {
        selected <- choices[1]
      }
      if (isTRUE(is.null(rvals[["attrGroups"]]))) {
        selection2 <- items2
      } else {
        selection2 <- rvals[["attrGroups"]]
      }
      tagList(
        selectInput(
          "anid",
          "Select ID column (e.g., \"anid\"):",
          choices,
          multiple = FALSE,
          selected = selected
        ),
        selectInput(
          "attrGroups",
          "Select group/cluster column:",
          items.all,
          multiple = F,
          selected = selection2[1]
        )
      )
    })
  })

  # Render select lookup for choosing groups to include
  output$subSelect <- renderUI({
    req(input$attrGroups)
    # print("subselect")
    quietly(label = "subselect", {
      df <- rvals$datasets[[rvals$selectedData]]
      items.all <- quietly(label = "items.all", df %>%
        dplyr::select(tidyselect::any_of(input$attrGroups)) %>% dplyr::pull() %>% unique() %>% sort())
      print("subSelect")
      if (isTRUE(is.null(rvals[["attrGroupsSub"]]))) {
        selection <- items.all
      } else {
        selection <- rvals[["attrGroupsSub"]]
      }
      if (!selection[1] %in% items.all) selection <- items.all
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
    req(nrow(rvals$datasets[[rvals$selectedData]]) > 0)
    # print("chem")
    # print("existing rvals$chem:")
    # print(rvals$chem)
    df <- rvals$datasets[[rvals$selectedData]]
    dfNum <- rvals$dfNum
    # Remove non-numeric columns from selection
    nums1 <- names(dfNum)
    # identify if any columns are elements and limit choices to those elements if one or more found
    num_def <- nums1
    if (any(tolower(num_def) %in% tolower(load_elements()$Symbol))) {
      num_def <- num_def[which(tolower(num_def) %in% tolower(load_elements()$Symbol))]
    }
    if (any(tolower(num_def) %in% tolower(load_elements()$Element))) {
      num_def <- num_def[which(tolower(num_def) %in% tolower(load_elements()$Element))]
    }
    quietly(label = "chem", {
      choices <- nums1[which(nums1 != "rowid")]
      if (length(num_def) == 0) num_def <- choices
      selectInput(
        "chem",
        "Select numeric attributes (e.g., element concentrations):",
        choices,
        multiple = TRUE,
        selected = num_def
      )
    })
  })

  # Render button to update datatable based on variable selections
  output$actionUI <- renderUI({
    req(nrow(rvals$datasets[[input$selectedData]]) > 0)
    # print("actionUI")
    n <- length(rvals$datasets)
    tmpName <- paste0("dataset_", n + 1)
    quietly(label = "actionUI", {
      tagList(
        uiOutput("impute.options"),
        br(),
        uiOutput("transform.options"),
        br(),
        checkboxInput("runPCA", "check to run PCA", value = F),
        checkboxInput("runUMAP", "check to run UMAP", value = F),
        checkboxInput("runLDA", "check to run LDA", value = F),
        textInput("datasetName", "Dataset Name", value = tmpName),
        actionButton("action", "Press to confirm selections", class = "mybtn"),
        hr()
      )
    })
  })

  # Render options for data imputation
  output$impute.options <- renderUI({
    req(nrow(rvals$importedData) > 0)
    print("impute.options")
    if (is.null(rvals$impute.method)) {
      sel <- "none"
    } else {
      sel <- rvals$impute.method
    }
    quietly(label = "impute.options", {
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
    if (is.null(rvals$transform.method)) {
      sel <- "none"
    } else {
      sel <- rvals$transform.method
    }
    quietly(label = "transform.options", {
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

  # output$resetUI <- renderUI({
  #   req(nrow(rvals$importedData) > 0)
  #   print("resetUI")
  #   quietly(label = "resetUI", {
  #     tagList(
  #       actionButton("resetElements", "Reset elements to original", class = "mybtn"),
  #       # actionButton("saveTbl", "Save data to new table", class = "mybtn"),
  #       actionButton("resetClear", "Clear workspace", class = "mybtn"),
  #       hr()
  #     )
  #   })
  # })

  # observeEvent(input$resetElements, {
  #   print("resetElements")
  #   rvals$datasets[[input$selectedData]][, rvals$chem] <- rvals$importedData[, rvals$chem]
  # })

  # observeEvent(input$saveTbl,{
  #   print("saveTbl")
  #   if(isTruthy(credentials$status)){
  #     newtbl = rvals$datasets$selectedData %>%
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

  observeEvent(input$resetClear, {
    print("resetClear")
    showModal(modalDialog(title = "Confirm", shiny::p("Press to confirm. All data will be lost"), footer = tagList(actionButton("confirmResetClear", "confirm"), modalButton("cancel")), easyClose = T))
  })

  observeEvent(input$confirmResetClear, {
    print("confirmResetClear")
    removeModal()
    session$reload()
  })

  # create subset data frame
  observeEvent(input$action, {
    # print("action")
    req(nrow(rvals$datasets[[input$selectedData]]) > 0)
    req(input$chem)

    # rvals$attrs <- unique(c(rvals$anid, rvals$attrGroups, "imputation", "transformation"))
    input_names <- c(
      "chem", "attrGroups", "anid", "runPCA",
      "runLDA", "runUMAP", "attrGroupsSub",
      "transform.method", "impute.method"
    )
    for (nm in unique(c(input_names, rvals$inputVars))) {
      rvals[[nm]] <- tryCatch(input[[nm]], error = function(e) NULL)
    }

    message("subsetting data")

    # print(rvals$chem)
    # print(rvals$importedData)
    # print(input$attrGroups)
    # print(input$anid)

    new <-
      tryCatch(rvals$importedData %>%
        dplyr::select(
          rowid,
          tidyselect::any_of(rvals$attrGroups),
          tidyselect::any_of(rvals$chem),
          tidyselect::any_of(rvals$anid)
        ) %>%
        dplyr::mutate_at(dplyr::vars(rvals$attrGroups), factor) %>%
        dplyr::mutate_at(dplyr::vars(rvals$chem), quietly(as.numeric)), error = function(e) mynotification(e))
    rvals$chem <- rvals$chem[which(rvals$chem %in% colnames(new))]
    message("data subsetted")

    # imputation
    quietly(label = "impute", {
      if (isTruthy(rvals$impute.method %in% c("rf", "pmm", "midastouch"))) {
        message("imputing")
        transformed <- new
        transformed <- tryCatch(mice::complete(mice::mice(transformed, method = rvals$impute.method)),
          error = function(e) {
            mynotification(e)
            return(rvals$datasets$selectedData[, rvals$chem])
          }
        )
        if (is.data.frame(transformed)) {
          transformed <- transformed %>%
            dplyr::mutate(imputation = rvals$impute.method)
          new[, rvals$chem] <- transformed
        } else {
          mynotification("imputation failed")
        }
        mynotification("imputed data")
      }
    })


    # Transforming data
    quietly(label = "transform", {
      if (rvals$transform.method != "none") {
        message("transforming")
        suppressWarnings({
          transformed <- new[, rvals$chem]
          if (rvals$transform.method == "zscale") {
            transformed <- zScale(transformed)
          } else if (rvals$transform.method %in% c("log10", "log")) {
            transformed <- transformed %>%
              dplyr::mutate_all(rvals$transform.method) %>%
              dplyr::mutate_all(round, digits = 3)
          }
          # get rid of infinite values
          transformed <- transformed %>%
            dplyr::mutate_all(list(function(c) {
              dplyr::case_when(!is.finite(c) ~ 0, TRUE ~ c)
            }))
          transformed <- transformed %>%
            dplyr::mutate(transformation = rvals$transform.method)
          new[, rvals$chem] <- transformed
          mynotification("transformed data")
        })
      }
    })

    if (isTRUE(is.null(input$attrGroupsSub))) {
      mynotification("Cannot proceed without any groups selected", type = "error")
    } else {
      message("filtering")
      quietly(label = "keep", {
        new <- new %>%
          dplyr::filter(!!as.name(input$attrGroups) %in% input$attrGroupsSub)
      })
    }

    message("dropping factor levels")
    new <- quietly(new %>%
      dplyr::mutate_at(dplyr::vars(input$attrGroups), as.character) %>%
      dplyr::mutate_at(dplyr::vars(input$attrGroups), factor))

    if (isTruthy(length(input$datasetName) > 0)) {
      rvals$datasets[[input$datasetName]] <- new
    } else {
      rvals$datasets[[paste0("dataset_", length(rvals$datasets) + 1)]] <- new
    }


    try({
      rvals$runPCAx <- input$runPCA
      rvals$runLDAx <- input$runLDA
      rvals$runUMAPx <- input$runUMAP
      # if(isTRUE(rvals$runPCA)) mynotification("Ran PCA")
      # if(isTRUE(rvals$runUMAP)) mynotification("Ran UMAP")
      # if(isTRUE(rvals$runLDA)) mynotification("Ran LDA")
    })

    # update current table
    updateCurrent(
      rvals,
      con,
      credentials,
      input,
      output,
      session
    )
    mynotification("updated", duration = 3)
  })

  observeEvent(rvals$datasets[[input$selectedData]], {
    req(nrow(rvals$importedData) > 0)
    print("restoring state")
    restoreState(rvals = rvals, input = input, session = session)
  })

  output$newCol <- renderUI({
    req(nrow(rvals$importedData) > 0)
    tagList(
      fluidRow(
        hr(),
        column(
          10,
          actionButton("addNewCol", "Add New Column", class = "mybtn")
        ),
        column(
          2,
          help_icon("", "If there is no grouping column in your data, you can add one here, or if you want to create a new grouping column (e.g., for clusters)."),
        )
      ),
      hr()
    )
  })

  observeEvent(input$addNewCol, {
    print("adding new column")
    showModal(modalDialog(
      textInput("createGroup", "New Group Name", value = "cluster"),
      textInput("createGroupVal", "New Group Default Value", value = "1"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("createSubmit", "Submit")
      )
    ))
  })

  observeEvent(input$createSubmit, {
    removeModal()
    # print("adding column")
    if (isTRUE(is.null(input$createGroup))) newCol <- "cluster" else newCol <- input$createGroup
    if (isTRUE(is.null(input$createGroupVal))) val <- "0" else val <- input$createGroupVal
    # rvals$importedData <- quietly(label = "addNewCol", {
    #   rvals$importedData %>%
    #     dplyr::select(-tidyselect::any_of(newCol)) %>%
    #     dplyr::mutate(!!as.name(newCol) := factor(as.character(val)))
    # })
    rvals$datasets[[input$selectedData]] <- quietly(label = "addNewCol2", {
      rvals$datasets[[input$selectedData]] %>%
        dplyr::select(-tidyselect::any_of(newCol)) %>%
        dplyr::mutate(!!as.name(newCol) := factor(as.character(val)))
    })
    updateCurrent(rvals, con, credentials, input, output, session)

    # Loop dynamically and assign each to rvals
    for (nm in rvals$inputVars) {
      rvals[[nm]] <- tryCatch(input[[nm]], error = function(e) NULL)
    }
  })
}
