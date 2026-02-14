
#' UI elements for group membership tab
#'
#' @return UI
#' @export
#'
#' @examples
#' groupTab()
groupTab = function(){
  tabPanel(title = "Probabilities and Distances",
           id = "groupMembershiptab",
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
  membership_proxy <- DT::dataTableProxy("membershipTbl")
  selected_membership_rowids <- shiny::reactiveVal(character())

  build_membership_display_table <- function(df) {
    add_checkbox_column(
      df = df,
      checked_rowids = selected_membership_rowids(),
      rowid_col = "rowid",
      checkbox_col = ".select",
      checkbox_class = "membership-row-check"
    )
  }

  get_source_features <- function(df, source) {
    if (!is.data.frame(df) || nrow(df) == 0) return(character())
    if (identical(source, "principal components")) {
      cols <- grep("^PC[0-9]+$", names(df), value = TRUE)
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
    tbl = NULL
    quietly(label = "render group size",{
      tbl = table(rvals$selectedData[[rvals$attrGroups]]) %>%
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
    if(isTruthy(!is.null(rvals$attrs))){
      eligible = tryCatch(getEligible(source_df, chem = source_features, group = rvals$attrGroups),error = function(e) return(NULL))
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
    choiceLengths = sapply(choices,function(x) length(unique(source_df[[x]])))
    choices = choices[which(choiceLengths == nrow(source_df))]
    if(isTruthy(!is.null(rvals$sampleID))){
      selected = rvals$sampleID
    } else {
      if(isTruthy("anid" %in% tolower(choices))){
        selected = choices[which(tolower(choices) == "anid")]
      } else {
        selected = choices[1]
      }
    }
    selectInput("sampleID","Choose sample ID Column",choices = choices,selected = selected)
  })

  output$gGroupAssignChoiceUI <- renderUI({
    req(rvals$selectedData)
    req(rvals$attrGroups)
    groups <- available_group_assignments(rvals$selectedData, rvals$attrGroups)
    selected_choice <- tryCatch(as.character(input$gGroupAssignChoice[[1]]), error = function(e) "")
    build_group_assignment_ui(
      choice_input_id = "gGroupAssignChoice",
      new_input_id = "gGroupAssignNew",
      groups = groups,
      selected_choice = selected_choice
    )
  })

  observeEvent(input$membershipRun,{
    req(rvals$attrGroups)
    mynotification("calculating membership")
    rvals$eligibleGroups = input$eligibleGroups
    rvals$sampleID = input$sampleID
    source_data <- get_membership_data(input$membershipDataset)
    if (is.null(source_data)) return(invisible(NULL))
    df <- suppressWarnings(source_data$df %>% dplyr::mutate_at(dplyr::vars(source_data$features), as.numeric))
    feature_cols <- source_data$features
    if (!input$sampleID %in% names(df)) {
      mynotification("Selected sample ID column is not available in this dataset source.", type = "error")
      return(invisible(NULL))
    }
    selected_data_with_rowid <- rvals$selectedData
    if (!"rowid" %in% names(selected_data_with_rowid)) {
      selected_data_with_rowid <- tibble::rowid_to_column(selected_data_with_rowid, var = "rowid")
    }
    rvals$membershipProbs = group.mem.probs(
      data = df,
      chem = feature_cols,
      group = rvals$attrGroups,
      eligible = input$eligibleGroups,
      method = input$membershipMethod,
      ID = input$sampleID
    )
    if (inherits(rvals$membershipProbs,"data.frame")){
      selected_membership_rowids(character())
      rvals$membershipProbs = rvals$membershipProbs %>%
        dplyr::mutate_at(dplyr::vars(ID), as.character) %>%
        dplyr::left_join(
          selected_data_with_rowid %>% dplyr::select(rowid, tidyselect::all_of(input$sampleID)) %>% dplyr::mutate_at(dplyr::vars(rowid), as.character),
          by = dplyr::join_by("ID" == !!input$sampleID)
        )
      rvals$membershipProbs <- rvals$membershipProbs %>%
        dplyr::mutate(
          ProjectionIncluded = dplyr::if_else(
            GroupVal %in% input$eligibleGroups,
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

    hide_by_default <- which(names(rvals$membershipProbs) %in% c("Group", "rowid", "BestValue"))
    right_align_cols <- which(names(rvals$membershipProbs) %in% c(input$eligibleGroups, "BestValue"))
    # Shift hidden/right-aligned targets by one because .select is first.
    hide_targets <- sort(unique(hide_by_default + 1))
    right_align_targets <- right_align_cols + 1

    DT::datatable(
      display_tbl,
      filter = "top",
      rownames = F,
      selection = 'none',
      style = 'default',
      class = "compact membership-plain-table nowrap",
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
        autoWidth = TRUE,
        scrollY = "420px",
        scrollCollapse = TRUE,
        scrollX = TRUE,
        paging = FALSE,
        columnDefs = list(
          list(visible = FALSE, targets = hide_targets - 1),
          list(className = "dt-right", targets = right_align_targets - 1),
          list(orderable = FALSE, searchable = FALSE, width = "32px", targets = 0),
          list(width = "78px", targets = "_all")
        )
      )
    )
  })

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
      col = rvals$attrGroups,
      value = values,
      rvals = rvals,
      con = con,
      credentials = credentials,
      input = input,
      output = output,
      session = session
    )

    if (is.data.frame(rvals$membershipProbs) && nrow(rvals$membershipProbs) > 0) {
      DT::replaceData(
        membership_proxy,
        build_membership_display_table(rvals$membershipProbs),
        resetPaging = FALSE,
        rownames = FALSE
      )
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
