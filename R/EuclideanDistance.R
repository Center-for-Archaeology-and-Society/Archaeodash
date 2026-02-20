#' Euclidean Distance UI
#'
#' @return UI
#' @export
#'
#' @examples
#' euclideanDistanceTab()
euclideanDistanceTab = function() {
  tabPanel(title = "Euclidean Distance",
           id = "euclideanDistancetab",
           fluidPage(
             wellPanel(
               h4("Euclidean Distance Controls"),
               fluidRow(
                 column(
                   3,
                   selectInput(
                     "EDdataset",
                     "Select dataset to use",
                     choices = c("elements", "principal components", "UMAP", "linear discriminants"),
                     selected = "elements"
                   )
                 ),
                 column(3, uiOutput("projectionGroupUI")),
                 column(3, uiOutput("EDsampleIDUI")),
                 column(
                   3,
                   radioButtons(
                     inputId = "EDmethod",
                     label = "Project within group?", choices = c(TRUE, FALSE),
                     selected = FALSE
                   )
                 )
               ),
               fluidRow(
                 column(
                   4,
                   sliderInput(
                     "EDlimit",
                     "Number of closest matches to return for each observation",
                     min = 1,
                     max = 100,
                     value = 10
                   )
                 ),
                 column(
                   4,
                   uiOutput("EDPCCountUI")
                 ),
                 column(
                   4,
                   tags$div(style = "margin-top: 20px;", actionButton("EDRun", "Calculate", class = "mybtn"))
                 )
               )
             ),
             wellPanel(
               fluidRow(
                 column(4, actionButton(
                   "edAssignMatchGroup", "Assign Match Group", class = "mybtn"
                 )),
                 column(
                   4,
                   actionButton("edChangeGroup", "Change Group Assignment", class = "mybtn")
                 ),
                 column(
                   4,
                   tags$div(style = "margin-top: 10px;", uiOutput("edGroupAssignChoiceUI"))
                 )
               )
             ),
             br(),
             fluidRow(
               column(
                 12,
                 div(
                   class = "membership-table-scroll-box",
                   fluidRow(
                     column(
                       4,
                       checkboxInput("edCompact", "Compact table", value = TRUE)
                     )
                   ),
                   DT::DTOutput("EDTbl")
                 )
               )
             )
           )
  ) # end tab panel
}

#' Euclidean Distance Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rvals reactive values object
#'
#' @return server
#' @export
#'
#' @examples
#' euclideanDistanceSrvr(input,output,session,rvals)
euclideanDistanceSrvr = function(input,output,session,rvals,credentials, con) {
  edProxy = DT::dataTableProxy('EDTbl')
  selected_ed_rowids <- shiny::reactiveVal(character())

  build_ed_display_table <- function(df) {
    checked_rowids <- shiny::isolate(selected_ed_rowids())
    add_checkbox_column(
      df = df,
      checked_rowids = checked_rowids,
      rowid_col = "rowid",
      checkbox_col = ".select",
      checkbox_class = "ed-row-check"
    )
  }

  get_checked_ed_rows <- function() {
    req(rvals$edistance)
    checked_rowids <- selected_ed_rowids()
    if (length(checked_rowids) == 0) return(integer())
    which(as.character(rvals$edistance$rowid) %in% checked_rowids)
  }

  apply_ed_assignment <- function(values) {
    req(rvals$edistance)
    selected_rows <- get_checked_ed_rows()
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      mynotification("Check one or more rows in Euclidean Distance results first.", type = "warning")
      return(invisible(NULL))
    }
    if (length(values) == 1) values <- rep(values, length(selected_rows))
    if (length(values) != length(selected_rows)) {
      mynotification("Assignment value count does not match checked rows.", type = "error")
      return(invisible(NULL))
    }
    rowid <- as.character(rvals$edistance$rowid[selected_rows])
    if (length(rowid) == 0 || !all(rowid %in% as.character(rvals$importedData$rowid))) {
      mynotification("Checked rows could not be mapped back to the dataset.", type = "error")
      return(invisible(NULL))
    }
    replaceCell(
      rowid = rowid,
      col = rvals$attrGroups,
      value = values,
      rvals = rvals,
      con = con,
      credentials = credentials,
      input = input,
      output = output,
      session = session
    )
    if (is.data.frame(rvals$edistance) && nrow(rvals$edistance) > 0) {
      DT::replaceData(edProxy, build_ed_display_table(rvals$edistance), resetPaging = FALSE, rownames = FALSE)
    }
    invisible(NULL)
  }

  get_ed_source_features <- function(df, source) {
    if (!is.data.frame(df) || nrow(df) == 0) return(character())
    if (identical(source, "principal components")) {
      cols <- pc_columns_sorted(grep("^PC[0-9]+$", names(df), value = TRUE))
    } else if (identical(source, "UMAP")) {
      cols <- grep("^V[0-9]+$", names(df), value = TRUE)
    } else if (identical(source, "linear discriminants")) {
      cols <- grep("^LD[0-9]+$", names(df), value = TRUE)
    } else {
      cols <- intersect(rvals$chem, names(df))
    }
    if (length(cols) == 0) {
      meta_cols <- unique(c(rvals$attrs, rvals$attrGroups, "rowid"))
      candidate_cols <- setdiff(names(df), meta_cols)
      numeric_cols <- candidate_cols[vapply(df[candidate_cols], is.numeric, logical(1))]
      cols <- numeric_cols
    }
    cols
  }

  get_ed_data <- function(source, notify = TRUE) {
    warn <- function(msg) {
      if (isTRUE(notify)) mynotification(msg, type = "warning")
    }
    if (identical(source, "principal components")) {
      if (!is.data.frame(rvals$pcadf) || nrow(rvals$pcadf) == 0) {
        warn("No PCA results available. Run confirm selections with PCA enabled.")
        return(NULL)
      }
      df <- rvals$pcadf
    } else if (identical(source, "UMAP")) {
      if (!is.data.frame(rvals$umapdf) || nrow(rvals$umapdf) == 0) {
        warn("No UMAP results available. Run confirm selections with UMAP enabled.")
        return(NULL)
      }
      df <- rvals$umapdf
    } else if (identical(source, "linear discriminants")) {
      if (!is.data.frame(rvals$LDAdf) || nrow(rvals$LDAdf) == 0) {
        warn("No LDA results available. Run confirm selections with LDA enabled.")
        return(NULL)
      }
      df <- rvals$LDAdf
    } else {
      df <- rvals$selectedData
    }
    if (!"rowid" %in% names(df)) {
      df <- tibble::rowid_to_column(df, var = "rowid")
    }
    feature_cols <- get_ed_source_features(df, source)
    if (length(feature_cols) == 0) {
      if (isTRUE(notify)) mynotification("No numeric analysis columns found for this dataset source.", type = "error")
      return(NULL)
    }
    df <- suppressWarnings(df %>% dplyr::mutate_at(dplyr::vars(feature_cols), as.numeric))
    list(df = df, features = feature_cols)
  }

  output$projectionGroupUI = renderUI({
    source_data <- get_ed_data(if (is.null(input$EDdataset)) "elements" else input$EDdataset, notify = FALSE)
    req(!is.null(source_data))
    if(!is.null(rvals$attrGroups)){
      choices = tryCatch(sort(unique(as.character(source_data$df[[rvals$attrGroups]]))),error = function(e) return(NULL))
    } else {
      choices = NULL
    }

    selectInput(
      "projectionGroup",
      "Select groups to project",
      choices = choices,
      selected = choices,
      multiple = T
    )
  })

  output$EDsampleIDUI = renderUI({
    source_data <- get_ed_data(if (is.null(input$EDdataset)) "elements" else input$EDdataset, notify = FALSE)
    req(!is.null(source_data))
    source_df <- source_data$df
    feature_cols <- source_data$features
    quietly(label = "rendering sample ID UI",{
      if(!is.null(rvals$attrs)){
        choices = tryCatch(names(source_df %>% dplyr::select(-tidyselect::any_of(feature_cols))),error = function(e) return(NULL))
      } else {
        choices = NULL
      }
      if ("rowid" %in% names(source_df)) {
        choices <- unique(c("rowid", choices))
      }
      choiceLengths = sapply(choices,function(x) length(unique(source_df[[x]])))
      choices = choices[which(choiceLengths == nrow(source_df))]
      if (length(choices) == 0 && "rowid" %in% names(source_df)) {
        choices <- "rowid"
      }
      if("anid" %in% tolower(choices)){
        selected = choices[which(tolower(choices) == "anid")]
      } else if ("rowid" %in% choices) {
        selected = "rowid"
      } else {
        selected = choices[1]
      }
      selectInput("edsampleID","Choose sample ID Column",choices = choices, selected = selected[1])
    })
  })

  output$EDPCCountUI = renderUI({
    req(input$EDdataset)
    if (!identical(input$EDdataset, "principal components")) {
      return(
        tags$small(
          class = "text-muted",
          "PC count selector appears when dataset is set to principal components."
        )
      )
    }
    if (!is.data.frame(rvals$pcadf) || nrow(rvals$pcadf) == 0) {
      return(
        tags$small(
          class = "text-muted",
          "Run Confirm Selections with PCA enabled to populate principal components."
        )
      )
    }
    pc_cols <- pc_columns_sorted(names(rvals$pcadf))
    if (length(pc_cols) == 0) {
      return(
        tags$small(
          class = "text-muted",
          "No PC columns were found in the selected PCA dataset."
        )
      )
    }
    choices <- membership_pc_count_choices(rvals$pca, pc_cols)
    default_count <- as.character(length(pc_cols))
    if (!(default_count %in% unname(choices))) {
      default_count <- unname(choices)[[length(choices)]]
    }
    selectInput(
      "EDPCCount",
      label = bslib::popover(
        tagList(
          "Number of PCs to use",
          trigger = bsicons::bs_icon("info-circle", title = "Help")
        ),
        title = "PC label format",
        "Values are shown as (PC variance / cumulative variance)."
      ),
      choices = choices,
      selected = default_count
    )
  })

  output$edGroupAssignChoiceUI <- renderUI({
    req(rvals$selectedData)
    req(rvals$attrGroups)
    groups <- available_group_assignments(rvals$selectedData, rvals$attrGroups)
    selected_choice <- tryCatch(as.character(input$edGroupAssignChoice[[1]]), error = function(e) "")
    build_group_assignment_ui(
      choice_input_id = "edGroupAssignChoice",
      new_input_id = "edGroupAssignNew",
      groups = groups,
      selected_choice = selected_choice
    )
  })

  observeEvent(input$EDRun,{
    quietly(label = "running Euclidean Distance",{
      source_data <- get_ed_data(if (is.null(input$EDdataset)) "elements" else input$EDdataset, notify = TRUE)
      if (is.null(source_data)) return(invisible(NULL))
      analysis_df <- source_data$df
      feature_cols <- source_data$features
      if (identical(input$EDdataset, "principal components")) {
        feature_cols <- limit_pc_features(feature_cols, input$EDPCCount)
      }
      ed_sample_id_col <- tryCatch(as.character(input$edsampleID[[1]]), error = function(e) "")
      if (is.null(ed_sample_id_col) || length(ed_sample_id_col) == 0 || is.na(ed_sample_id_col[[1]])) {
        ed_sample_id_col <- ""
      } else {
        ed_sample_id_col <- ed_sample_id_col[[1]]
      }
      if (!nzchar(ed_sample_id_col) || !(ed_sample_id_col %in% names(analysis_df))) {
        if ("rowid" %in% names(analysis_df)) {
          ed_sample_id_col <- "rowid"
          mynotification("Using rowid as sample ID for this dataset source.", type = "warning")
        } else {
          mynotification("Selected sample ID column is not available in this dataset source.", type = "error")
          return(invisible(NULL))
        }
      }
      mynotification("calculating Euclidean Distances")
      projection_groups <- input$projectionGroup
      if (is.null(projection_groups) || length(projection_groups) == 0) {
        projection_groups <- sort(unique(as.character(analysis_df[[rvals$attrGroups]])))
        projection_groups <- projection_groups[!is.na(projection_groups) & nzchar(projection_groups)]
        if (length(projection_groups) > 0) {
          mynotification("Projection groups were not selected; using all groups.", type = "warning")
        }
      }
      if (is.null(projection_groups) || length(projection_groups) == 0) {
        mynotification("Choose at least one projection group.", type = "warning")
        return(invisible(NULL))
      }
      rvals$edistance <- calcEDistance(
        data = analysis_df,
        projection = projection_groups,
        id = ed_sample_id_col,
        attrGroups = rvals$attrGroups,
        chem = feature_cols,
        limit = input$EDlimit,
        withinGroup = input$EDmethod
      )
      if (!is.data.frame(rvals$edistance)) {
        mynotification("Euclidean Distance did not return a result table.", type = "error")
        return(invisible(NULL))
      }
      selected_ed_rowids(character())
      mynotification("completed calculation")
    })
  })

  output$EDTbl = DT::renderDataTable({
    req(rvals$edistance)
    quietly(label = "rendering Euclidean Distance table",{
      compact_mode <- isTRUE(input$edCompact)
      sort_col <- if (!is.null(input$edsampleID) && input$edsampleID %in% names(rvals$edistance)) {
        input$edsampleID
      } else {
        names(rvals$edistance)[which(names(rvals$edistance) != "rowid")][1]
      }
      display_tbl <- rvals$edistance %>%
        dplyr::mutate_at(dplyr::vars(distance), as.numeric) %>%
        dplyr::arrange(!!as.name(sort_col),distance)
      display_tbl <- build_ed_display_table(display_tbl)
      hide_by_default <- which(names(display_tbl) %in% c("rowid"))
      right_align_cols <- which(names(display_tbl) %in% c("distance"))
      dt <- DT::datatable(
        display_tbl,
        filter = "top",
        rownames = FALSE,
        selection = "none",
        style = "default",
        class = paste(
          if (compact_mode) "compact" else "",
          "membership-plain-table",
          if (compact_mode) "membership-compact-table" else "membership-fullwidth-table",
          "nowrap"
        ),
        extensions = c("Buttons"),
        escape = FALSE,
        callback = DT::JS(
          "table.on('change', 'input.ed-row-check', function(){",
          "  var checked = [];",
          "  table.$('input.ed-row-check:checked').each(function(){",
          "    checked.push(String($(this).data('rowid')));",
          "  });",
          "  Shiny.setInputValue('ed_checked_rowids', checked, {priority: 'event'});",
          "});"
        ),
        options = list(
          dom = "Brt",
          buttons = list("colvis"),
          autoWidth = !compact_mode,
          scrollY = "420px",
          scrollCollapse = TRUE,
          scrollX = TRUE,
          paging = FALSE,
          columnDefs = list(
            list(visible = FALSE, targets = hide_by_default - 1),
            list(className = "dt-right", targets = right_align_cols - 1),
            list(orderable = FALSE, searchable = FALSE, width = "32px", targets = 0)
          )
        )
      )
      if (compact_mode) {
        dt$x$options$columnDefs <- c(
          dt$x$options$columnDefs,
          list(list(width = "78px", targets = "_all"))
        )
      }
      if ("distance" %in% names(display_tbl)) {
        dt <- DT::formatRound(dt, columns = "distance", digits = 4)
      }
      dt
    })
  }, server = FALSE)
  outputOptions(output, "EDTbl", suspendWhenHidden = FALSE)

  observeEvent(input$ed_checked_rowids, {
    rowids <- as.character(input$ed_checked_rowids)
    rowids <- rowids[!is.na(rowids) & nzchar(rowids)]
    selected_ed_rowids(unique(rowids))
  }, ignoreNULL = FALSE)

  observeEvent(input$edAssignMatchGroup,{
    quietly(label = "assigning match group",{
      selRows <- get_checked_ed_rows()
      if (is.null(selRows) || length(selRows) == 0) {
        mynotification("Check one or more rows in Euclidean Distance results first.", type = "warning")
        return(invisible(NULL))
      }
      match_col <- paste0(rvals$attrGroups, "_match")
      if (!match_col %in% names(rvals$edistance)) {
        mynotification("Unable to locate matched-group column in Euclidean Distance results.", type = "error")
        return(invisible(NULL))
      }
      assign_values <- as.character(rvals$edistance[[match_col]][selRows])
      apply_ed_assignment(assign_values)
      mynotification("Updated checked row assignments from match groups.", type = "message")
    })
  })

  observeEvent(input$edChangeGroup,{
    quietly(label = "assigning new group",{
      new_group <- resolve_group_assignment_target(input$edGroupAssignChoice, input$edGroupAssignNew)
      if (!nzchar(new_group)) {
        mynotification("Choose an existing group or enter a new group designation.", type = "warning")
        return(invisible(NULL))
      }
      apply_ed_assignment(new_group)
      mynotification("Updated checked row assignments.", type = "message")
    })
  })

}

#' Calculate Euclidean Distance
#'
#' @param data selected data
#' @param projection groups to project against
#' @param id ID column
#' @param attrGroups selected attribute group
#' @param chem chemical data
#' @param limit number of closest matches to return
#' @param withinGroup whether to match within the same group
#'
#' @return data frame of closest matches
#' @export
#'
#' @examples
#' calcEDistance(rvals$selectedData,input$projectionGroup,input$edsampleID,rvals$attrGroups,rvals$chem,input$EDlimit,input$EDmethod)
calcEDistance = function(data,projection,id,attrGroups,chem,limit,withinGroup){
  result <- NULL
  quietly(label = "calcEDistance",{
    within_group_flag <- isTRUE(as.logical(withinGroup))
    # Use rowid as stable keys so duplicate IDs do not break matrix column names.
    work <- data %>%
      dplyr::mutate(
        .rowid_chr = as.character(rowid),
        .id_chr = as.character(.data[[id]]),
        .group_chr = as.character(.data[[attrGroups]])
      )
    projection_rowids <- work %>%
      dplyr::filter(.group_chr %in% projection) %>%
      dplyr::pull(.rowid_chr)

    m <- work[, chem, drop = FALSE] %>% as.matrix()
    rownames(m) <- work$.rowid_chr
    d <- stats::dist(m, method = "euclidean")

    result <- as.data.frame(as.table(as.matrix(d)), stringsAsFactors = FALSE) %>%
      dplyr::rename(observation_rowid = "Var1", match_rowid = "Var2", distance = "Freq") %>%
      dplyr::mutate(
        observation_rowid = as.character(observation_rowid),
        match_rowid = as.character(match_rowid),
        distance = as.numeric(distance)
      ) %>%
      dplyr::filter(observation_rowid != match_rowid) %>%
      dplyr::filter(match_rowid %in% projection_rowids) %>%
      dplyr::group_by(observation_rowid) %>%
      dplyr::arrange(distance, .by_group = TRUE) %>%
      dplyr::slice_head(n = limit) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        work %>%
          dplyr::select(observation_rowid = .rowid_chr, observation = .id_chr, observationGroup = .group_chr),
        by = "observation_rowid"
      ) %>%
      dplyr::left_join(
        work %>%
          dplyr::select(match_rowid = .rowid_chr, match = .id_chr, matchGroup = .group_chr),
        by = "match_rowid"
      ) %>%
      dplyr::mutate(rowid = observation_rowid, .before = 1) %>%
      dplyr::select(tidyselect::any_of(c("rowid", "observation", "match", "distance", "observationGroup", "matchGroup"))) %>%
      dplyr::arrange(observation, distance)
    if (identical(id, "rowid")) {
      result <- result %>%
        dplyr::select(-tidyselect::any_of("observation")) %>%
        dplyr::rename(!!as.name(attrGroups) := observationGroup, !!as.name(paste0(attrGroups, "_match")) := matchGroup)
    } else {
      result <- result %>%
        dplyr::rename(
          !!as.name(id) := observation,
          !!as.name(attrGroups) := observationGroup,
          !!as.name(paste0(attrGroups, "_match")) := matchGroup
        )
    }
    if(!within_group_flag){
      result <- result %>%
        dplyr::filter(!!as.name(attrGroups) != !!as.name(paste0(attrGroups,"_match")))
    }
  })
  return(result)
}
