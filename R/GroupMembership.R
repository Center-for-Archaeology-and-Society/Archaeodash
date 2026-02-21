
#' UI elements for group membership tab
#'
#' @return UI
#' @export
#'
#' @examples
#' groupTab()

groupTab = function(){
  tabPanel(title = "Probabilities and Distances",
           value = "groupMembershiptab",
           fluidPage(
             fluidRow(
               column(
                 4,
                 wellPanel(
                   h4("Group Membership Controls"),
                   uiOutput("eligibleGroupUI"),
                   uiOutput("sampleIDUI"),
                   selectInput("membershipMethod","Select method",choices = c("Hotellings T2"="Hotellings","Mahalanobis distances"="Mahalanobis")),
                   selectInput(
                     "membershipDataset",
                     "select dataset to use",
                     choices = c("elements", "principal components", "UMAP", "linear discriminants"),
                     selected = "elements"
                   ),
                   uiOutput("membershipPCCountUI"),
                   actionButton("membershipRun","Calculate", class = "mybtn")
                 )
               ),
               column(
                 8,
                 h4("Group Sizes"),
                 DT::DTOutput("grpSizeTbl")
               )
             ),
             br(),
             h4("Membership Probabilities"),
           wellPanel(
               fluidRow(
                 column(4,
                        actionButton("gAssignBestGroup","Assign Best Group", class = "mybtn")
                 ),
                 column(4,
                        actionButton("gChangeGroup","Change Group Assignment", class = "mybtn")
                 ),
                 column(4,
                        tags$div(style = "margin-top: 10px;", uiOutput("gGroupAssignChoiceUI"))
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
                       checkboxInput("membershipCompact", "Compact table", value = TRUE)
                     )
                   ),
                   DT::DTOutput('membershipTbl')
                 )
               )
             )
           )
  ) # end group membership panel
}

#' Group Membership Server
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
#' groupServer(input,output,session,rvals)
groupServer = function(input,output,session,rvals, credentials, con){
  selected_membership_rowids <- shiny::reactiveVal(character())
  resolve_group_column <- function(df = NULL) {
    candidate <- tryCatch(as.character(rvals$attrGroups[[1]]), error = function(e) "")
    if (!nzchar(candidate)) {
      candidate <- tryCatch(as.character(input$attrGroups[[1]]), error = function(e) "")
    }
    if (!is.data.frame(df) || nrow(df) == 0) return(candidate)
    if (nzchar(candidate) && candidate %in% names(df)) return(candidate)
    non_numeric <- names(df)[!vapply(df, is.numeric, logical(1))]
    if (length(non_numeric) > 0) return(non_numeric[[1]])
    names(df)[[1]]
  }

  build_membership_display_table <- function(df) {
    checked_rowids <- shiny::isolate(selected_membership_rowids())
    add_checkbox_column(
      df = df,
      checked_rowids = checked_rowids,
      rowid_col = "rowid",
      checkbox_col = ".select",
      checkbox_class = "membership-row-check"
    )
  }

  get_source_features <- function(df, source) {
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
      meta_cols <- unique(c(rvals$attrs, rvals$attrGroups, "rowid", "ID", "BestGroup", "GroupVal"))
      candidate_cols <- setdiff(names(df), meta_cols)
      numeric_cols <- candidate_cols[vapply(df[candidate_cols], is.numeric, logical(1))]
      cols <- numeric_cols
    }
    cols
  }

  get_membership_data <- function(source) {
    if (identical(source, "principal components")) {
      if (!is.data.frame(rvals$pcadf) || nrow(rvals$pcadf) == 0) {
        mynotification("No PCA results available. Run confirm selections with PCA enabled.", type = "warning")
        return(NULL)
      }
      df <- rvals$pcadf
    } else if (identical(source, "UMAP")) {
      if (!is.data.frame(rvals$umapdf) || nrow(rvals$umapdf) == 0) {
        mynotification("No UMAP results available. Run confirm selections with UMAP enabled.", type = "warning")
        return(NULL)
      }
      df <- rvals$umapdf
    } else if (identical(source, "linear discriminants")) {
      if (!is.data.frame(rvals$LDAdf) || nrow(rvals$LDAdf) == 0) {
        mynotification("No LDA results available. Run confirm selections with LDA enabled.", type = "warning")
        return(NULL)
      }
      df <- rvals$LDAdf
    } else {
      df <- rvals$selectedData
    }
    features <- get_source_features(df, source)
    if (length(features) == 0) {
      mynotification("No numeric analysis columns found for this dataset source.", type = "error")
      return(NULL)
    }
    list(df = df, features = features)
  }

  ##### UI Outputs for membership groups ####

  output$grpSizeTbl = DT::renderDataTable({
    req(nrow(rvals$selectedData) > 0)
    group_col <- resolve_group_column(rvals$selectedData)
    req(nzchar(group_col))
    req(group_col %in% names(rvals$selectedData))
    tbl = NULL
    quietly(label = "render group size",{
      tbl = table(rvals$selectedData[[group_col]]) %>%
        as.data.frame() %>%
        setNames(c("Group","Count"))
    })
    DT::datatable(tbl, rownames = FALSE, options = list(dom = "t"))
  })

  output$eligibleGroupUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    source <- if (is.null(input$membershipDataset)) "elements" else input$membershipDataset
    source_df <- switch(
      source,
      "principal components" = rvals$pcadf,
      "UMAP" = rvals$umapdf,
      "linear discriminants" = rvals$LDAdf,
      rvals$selectedData
    )
    if (!is.data.frame(source_df) || nrow(source_df) == 0) {
      source_df <- rvals$selectedData
      source <- "elements"
    }
    source_features <- get_source_features(source_df, source)
    group_col <- resolve_group_column(source_df)
    req(nzchar(group_col))
    req(group_col %in% names(source_df))
    if(isTruthy(!is.null(rvals$attrs))){
      eligible = tryCatch(getEligible(source_df, chem = source_features, group = group_col),error = function(e) return(NULL))
    } else {
      eligible = NULL
    }
    if(isTruthy(is.null(rvals$eligibleGroups))){
      selected = eligible
    } else {
      selected = rvals$eligibleGroups
    }
    selectInput("eligibleGroups","Choose Eligible Groups",choices = eligible, multiple = T, selected = selected)
  })

  output$sampleIDUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    source <- if (is.null(input$membershipDataset)) "elements" else input$membershipDataset
    source_df <- switch(
      source,
      "principal components" = rvals$pcadf,
      "UMAP" = rvals$umapdf,
      "linear discriminants" = rvals$LDAdf,
      rvals$selectedData
    )
    if (!is.data.frame(source_df) || nrow(source_df) == 0) {
      source_df <- rvals$selectedData
    }
    choices = intersect(rvals$attrs, names(source_df))
    if ("rowid" %in% names(source_df)) {
      choices <- unique(c("rowid", choices))
    }
    choiceLengths = sapply(choices,function(x) length(unique(source_df[[x]])))
    choices = choices[which(choiceLengths == nrow(source_df))]
    if (length(choices) == 0 && "rowid" %in% names(source_df)) {
      choices <- "rowid"
    }
    if(isTruthy(!is.null(rvals$sampleID))){
      selected = as.character(rvals$sampleID[[1]])
    } else {
      if(isTruthy("anid" %in% tolower(choices))){
        selected = choices[which(tolower(choices) == "anid")]
      } else if ("rowid" %in% choices) {
        selected = "rowid"
      } else {
        selected = choices[1]
      }
    }
    selectInput("sampleID","Choose sample ID Column",choices = choices,selected = selected)
  })

  output$membershipPCCountUI = renderUI({
    req(input$membershipDataset)
    if (!identical(input$membershipDataset, "principal components")) {
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
      "membershipPCCount",
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

  output$gGroupAssignChoiceUI <- renderUI({
    req(rvals$selectedData)
    group_col <- resolve_group_column(rvals$selectedData)
    req(nzchar(group_col))
    req(group_col %in% names(rvals$selectedData))
    groups <- available_group_assignments(rvals$selectedData, group_col)
    selected_choice <- tryCatch(as.character(shiny::isolate(input$gGroupAssignChoice[[1]])), error = function(e) "")
    new_value <- tryCatch(as.character(shiny::isolate(input$gGroupAssignNew[[1]])), error = function(e) "")
    build_group_assignment_ui(
      choice_input_id = "gGroupAssignChoice",
      new_input_id = "gGroupAssignNew",
      groups = groups,
      selected_choice = selected_choice,
      new_value = new_value
    )
  })

  observeEvent(input$membershipRun,{
    mynotification("calculating membership")
    source_data <- get_membership_data(input$membershipDataset)
    if (is.null(source_data)) return(invisible(NULL))
    group_col <- resolve_group_column(source_data$df)
    if (!nzchar(group_col) || !(group_col %in% names(source_data$df))) {
      mynotification("No valid group column is available for membership calculation.", type = "error")
      return(invisible(NULL))
    }
    rvals$attrGroups <- group_col
    df <- suppressWarnings(source_data$df %>% dplyr::mutate_at(dplyr::vars(source_data$features), as.numeric))
    feature_cols <- source_data$features
    if (identical(input$membershipDataset, "principal components")) {
      feature_cols <- limit_pc_features(feature_cols, input$membershipPCCount)
    }
    eligible_groups <- tryCatch(as.character(input$eligibleGroups), error = function(e) character())
    eligible_groups <- eligible_groups[!is.na(eligible_groups) & nzchar(eligible_groups)]
    if (length(eligible_groups) == 0) {
      eligible_groups <- tryCatch(
        getEligible(df, chem = feature_cols, group = group_col),
        error = function(e) character()
      )
      eligible_groups <- as.character(eligible_groups)
      eligible_groups <- eligible_groups[!is.na(eligible_groups) & nzchar(eligible_groups)]
    }
    if (length(eligible_groups) == 0) {
      eligible_groups <- sort(unique(as.character(df[[rvals$attrGroups]])))
      eligible_groups <- eligible_groups[!is.na(eligible_groups) & nzchar(eligible_groups)]
      if (length(eligible_groups) > 0) {
        mynotification("Eligible groups were not selected; using all groups.", type = "warning")
      }
    }
    if (length(eligible_groups) == 0) {
      mynotification("No eligible groups are available for membership calculation.", type = "error")
      return(invisible(NULL))
    }
    rvals$eligibleGroups <- eligible_groups

    sample_id_col <- tryCatch(as.character(input$sampleID[[1]]), error = function(e) "")
    if (is.null(sample_id_col) || length(sample_id_col) == 0 || is.na(sample_id_col[[1]])) {
      sample_id_col <- ""
    } else {
      sample_id_col <- sample_id_col[[1]]
    }
    if (!nzchar(sample_id_col) || !(sample_id_col %in% names(df))) {
      if ("rowid" %in% names(df)) {
        sample_id_col <- "rowid"
        mynotification("Using rowid as sample ID for this dataset source.", type = "warning")
      } else {
        mynotification("Selected sample ID column is not available in this dataset source.", type = "error")
        return(invisible(NULL))
      }
    }
    rvals$sampleID <- sample_id_col
    selected_data_with_rowid <- rvals$selectedData
    if (!"rowid" %in% names(selected_data_with_rowid)) {
      selected_data_with_rowid <- tibble::rowid_to_column(selected_data_with_rowid, var = "rowid")
    }
    rvals$membershipProbs = group.mem.probs(
      data = df,
      chem = feature_cols,
      group = group_col,
      eligible = eligible_groups,
      method = input$membershipMethod,
      ID = sample_id_col
    )
    if (inherits(rvals$membershipProbs,"data.frame")){
      selected_membership_rowids(character())
      rvals$membershipProbs = rvals$membershipProbs %>%
        dplyr::mutate_at(dplyr::vars(ID), as.character) %>%
        dplyr::left_join(
          selected_data_with_rowid %>% dplyr::select(rowid, tidyselect::all_of(sample_id_col)) %>% dplyr::mutate_at(dplyr::vars(rowid), as.character),
          by = dplyr::join_by("ID" == !!sample_id_col)
        )
      rvals$membershipProbs <- rvals$membershipProbs %>%
        dplyr::mutate(
          ProjectionIncluded = dplyr::if_else(
            GroupVal %in% eligible_groups,
            "yes",
            "no"
          ),
          .after = "InGroup"
        )
      if(isTruthy(!"rowid" %in% names(rvals$membershipProbs))){
        rvals$membershipProbs = rvals$membershipProbs %>%
          dplyr::mutate(rowid = as.character(as.integer(ID)))
      }
      priority_cols <- unique(c("ID", "GroupVal", "BestGroup", input$eligibleGroups))
      rvals$membershipProbs <- rvals$membershipProbs %>%
        dplyr::select(tidyselect::any_of(priority_cols), tidyselect::everything())
      if(isTruthy(!is.null(rvals$membershipProbs))){
        mynotification("completed calculation")
      }
    } else {
      mynotification("error calculating membership", type = "error")
    }

  })

  output$membershipTbl = DT::renderDataTable({
    req(rvals$membershipProbs)
    display_tbl <- build_membership_display_table(rvals$membershipProbs)
    compact_mode <- isTRUE(input$membershipCompact)

    hide_by_default <- which(names(rvals$membershipProbs) %in% c("Group", "rowid", "BestValue"))
    right_align_cols <- which(names(rvals$membershipProbs) %in% c(input$eligibleGroups, "BestValue"))
    numeric_cols <- names(rvals$membershipProbs)[vapply(rvals$membershipProbs, is.numeric, logical(1))]
    # Shift hidden/right-aligned targets by one because .select is first.
    hide_targets <- sort(unique(hide_by_default + 1))
    right_align_targets <- right_align_cols + 1

    dt <- DT::datatable(
      display_tbl,
      filter = "top",
      rownames = F,
      selection = 'none',
      style = 'default',
      class = paste(
        if (compact_mode) "compact" else "",
        "membership-plain-table",
        if (compact_mode) "membership-compact-table" else "membership-fullwidth-table",
        "nowrap"
      ),
      extensions = c("Buttons"),
      escape = FALSE,
      callback = DT::JS(
        "table.on('change', 'input.membership-row-check', function(){",
        "  var checked = [];",
        "  table.$('input.membership-row-check:checked').each(function(){",
        "    checked.push(String($(this).data('rowid')));",
        "  });",
        "  Shiny.setInputValue('membership_checked_rowids', checked, {priority: 'event'});",
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
          list(visible = FALSE, targets = hide_targets - 1),
          list(className = "dt-right", targets = right_align_targets - 1),
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
    if (length(numeric_cols) > 0) {
      dt <- DT::formatRound(dt, columns = numeric_cols, digits = 2)
    }
    dt
  }, server = FALSE)
  outputOptions(output, "membershipTbl", suspendWhenHidden = FALSE)

  observeEvent(input$membership_checked_rowids, {
    rowids <- as.character(input$membership_checked_rowids)
    rowids <- rowids[!is.na(rowids) & nzchar(rowids)]
    selected_membership_rowids(unique(rowids))
  }, ignoreNULL = FALSE)

  get_checked_membership_rows <- function() {
    req(rvals$membershipProbs)
    checked_rowids <- selected_membership_rowids()
    if (length(checked_rowids) == 0) return(integer())
    which(as.character(rvals$membershipProbs$rowid) %in% checked_rowids)
  }

  apply_membership_assignment <- function(values) {
    req(rvals$membershipProbs)
    group_col <- resolve_group_column(rvals$importedData)
    if (!nzchar(group_col) || !(group_col %in% names(rvals$importedData))) {
      mynotification("Unable to determine a valid group column for assignment updates.", type = "error")
      return(invisible(NULL))
    }
    selected_rows <- get_checked_membership_rows()
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      mynotification("Check one or more rows in Membership Probabilities first.", type = "warning")
      return(invisible(NULL))
    }
    if (length(values) == 1) {
      values <- rep(values, length(selected_rows))
    }
    if (length(values) != length(selected_rows)) {
      mynotification("Assignment value count does not match selected rows.", type = "error")
      return(invisible(NULL))
    }

    rowid <- as.character(rvals$membershipProbs$rowid[selected_rows])
    if (length(rowid) == 0 || !all(rowid %in% as.character(rvals$importedData$rowid))) {
      mynotification("Selected rows could not be mapped back to the dataset.", type = "error")
      return(invisible(NULL))
    }

    replaceCell(
      rowid = rowid,
      col = group_col,
      value = values,
      rvals = rvals,
      con = con,
      credentials = credentials,
      input = input,
      output = output,
      session = session
    )

    if (is.data.frame(rvals$membershipProbs)) {
      rvals$membershipProbs <- rvals$membershipProbs
    }
    mynotification("Updated selected row assignments.", type = "message")
    invisible(NULL)
  }

  observeEvent(input$gAssignBestGroup,{
    quietly(label = "assigning best group",{
      req(rvals$membershipProbs)
      selected_rows <- get_checked_membership_rows()
      if (is.null(selected_rows) || length(selected_rows) == 0) {
        mynotification("Check one or more rows in Membership Probabilities first.", type = "warning")
        return(invisible(NULL))
      }
      if (!"BestGroup" %in% names(rvals$membershipProbs)) {
        mynotification("BestGroup column is missing from membership results.", type = "error")
        return(invisible(NULL))
      }
      best_values <- as.character(rvals$membershipProbs$BestGroup[selected_rows])
      apply_membership_assignment(best_values)
    })
  })

  observeEvent(input$gChangeGroup,{
    quietly(label = "assigning new group",{
      new_group <- resolve_group_assignment_target(input$gGroupAssignChoice, input$gGroupAssignNew)
      if (!nzchar(new_group)) {
        mynotification("Choose an existing group or enter a new group designation.", type = "warning")
        return(invisible(NULL))
      }
      apply_membership_assignment(new_group)
    })
  })

}
