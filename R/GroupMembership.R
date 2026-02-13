
#' UI elements for group membership tab
#'
#' @return UI
#' @export
#'
#' @examples
#' groupTab()
groupTab = function(){
  tabPanel(title = "Group Membership",
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
                   selectInput("membershipDataset","select dataset to use",choices =c('elements', 'principal components'), selected = 'elements'),
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
                        tags$div(
                          style = "margin-top: 10px;",
                          textInput("gNewGroup", NULL, placeholder = "Enter new group designation")
                        )
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
    if(isTruthy(!is.null(rvals$attrs))){
      eligible = tryCatch(getEligible(rvals$selectedData, chem = rvals$chem, group = rvals$attrGroups),error = function(e) return(NULL))
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
    choices = rvals$attrs
    choiceLengths = sapply(choices,function(x) length(unique(rvals$selectedData[[x]])))
    choices = choices[which(choiceLengths == nrow(rvals$selectedData))]
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

  observeEvent(input$membershipRun,{
    req(rvals$attrGroups)
    mynotification("calculating membership")
    rvals$eligibleGroups = input$eligibleGroups
    rvals$sampleID = input$sampleID
    if(isTruthy(input$membershipDataset == "elements")){
      df = rvals$selectedData
    } else {
      req(rvals$pcadf)
      df = rvals$pcadf
    }
    selected_data_with_rowid <- rvals$selectedData
    if (!"rowid" %in% names(selected_data_with_rowid)) {
      selected_data_with_rowid <- tibble::rowid_to_column(selected_data_with_rowid, var = "rowid")
    }
    rvals$membershipProbs = group.mem.probs(data = df,chem = rvals$chem, group = rvals$attrGroups,eligible = input$eligibleGroups,method = input$membershipMethod, ID = input$sampleID)
    if (inherits(rvals$membershipProbs,"data.frame")){
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
    hide_by_default <- which(names(rvals$membershipProbs) %in% c("Group", "rowid", "BestValue"))
    right_align_cols <- which(names(rvals$membershipProbs) %in% c(input$eligibleGroups, "BestValue"))
    hide_targets <- sort(unique(hide_by_default))

    DT::datatable(
      rvals$membershipProbs,
      filter = "top",
      rownames = F,
      selection = 'multiple',
      style = 'default',
      class = "compact membership-plain-table nowrap",
      extensions = c("Buttons"),
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
          list(className = "dt-right", targets = right_align_cols - 1),
          list(width = "78px", targets = "_all")
        )
      )
    )
  })

  observeEvent(input$gAssignBestGroup,{
    quietly(label = "assigning match group",{
      selRows = input$membershipTbl_rows_selected
      rvals$gNewValue = rvals$membershipProbs[[4]][selRows]
    })
  })

  observeEvent(input$gChangeGroup,{
    quietly(label = "assigning new group",{
      rvals$gNewValue = input$gNewGroup
    })
  })

  observeEvent(rvals$gNewValue, {
    message("updating group")
    quietly(label = "updating group",{
      if(isTruthy(!is.null(input$membershipTbl_state$length))){
        rvals$membershipTbl_state_length = input$membershipTbl_state$length
      } else {
        rvals$membershipTbl_state_length = 25
      }
      print("getting rowid")
      rowid = rvals$membershipProbs$rowid[input$membershipTbl_rows_selected]
      print(rowid)
      if(isTruthy(rowid %in% as.character(rvals$importedData$rowid))){
        print('replacing cell')
        replaceCell(rowid = rowid,col = rvals$attrGroups,value = rvals$gNewValue, rvals = rvals, con = con, credentials = credentials, input = input, output = output, session = session)
      } else {
        mynotification("rowid not found in imported data",type = "error")
      }
      rvals$gNewValue = NULL
      print('end')
    })
  })

  observeEvent(input$gNewGroup,{
    if(isTruthy(stringr::str_detect(input$gNewGroup,"[a-zA-z]|[0-9]"))){
      shinyjs::enable("gChangeGroup")
    } else {
      shinyjs::disable("gChangeGroup")
    }
  })

}
