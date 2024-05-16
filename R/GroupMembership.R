
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
           sidebarLayout(sidebarPanel(
             uiOutput("eligibleGroupUI"),
             uiOutput("sampleIDUI"),
             selectInput("membershipMethod","Select method",choices = c("Hotellings T2"="Hotellings","Mahalanobis distances"="Mahalanobis")),
             selectInput("dataset","select dataset to use",choices =c('elements', 'principal components'), selected = 'elements'),
             actionButton("membershipRun","Calculate", class = "mybtn"),
           ),
           mainPanel(
             tabsetPanel(id = "GroupMembershipPanels",
                         type = "pills",
                         tabPanel(title = "Group Sizes",
                                  id = "groupSizePanel",
                                  uiOutput("grpSizeUI")
                         ),
                         tabPanel(
                           title = "Membership Probabilities",
                           id = "membershipProbs",
                           wellPanel(
                             fluidRow(
                               column(4,
                                      actionButton("gAssignBestGroup","Assign Best Group", class = "mybtn")
                               ),
                               column(4, offset = 2,
                                      actionButton("gChangeGroup","Change Group Assignment", class = "mybtn"),
                                      textInput("gNewGroup","Enter new group designation")
                               )
                             )
                           ),
                           br(),
                           DT::DTOutput('membershipTbl')
                         )
             ) # end tabset panel
           ) # end main panel
           ) # end sidebar layout
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

  output$grpSizeUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    tbl = table(rvals$selectedData[[rvals$attrGroups]]) %>%
      as.data.frame() %>%
      setNames(c("Group","Count"))
    DT::renderDataTable(tbl)
  })

  output$eligibleGroupUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    if(!is.null(rvals$attrs)){
      eligible = tryCatch(getEligible(rvals$selectedData, chem = rvals$chem, group = rvals$attrGroups),error = function(e) return(NULL))
    } else {
      eligible = NULL
    }
    if(is.null(rvals$eligibleGroups)){
      selected = eligible
    } else {
      selected = rvals$eligibleGroups
    }
    selectInput("eligibleGroups","Choose Eligible Groups",choices = eligible, multiple = T, selected = selected)
  })

  output$sampleIDUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    choices = rvals$attrs
    if(!is.null(rvals$sampleID)){
      selected = rvals$sampleID
    } else {
      if("anid" %in% tolower(choices)){
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
    if(input$dataset == "elements"){
      df = rvals$selectedData
    } else {
      df = rvals$pcadf
    }
    rvals$membershipProbs = tryCatch(group.mem.probs(data = df,chem = rvals$chem, group = rvals$attrGroups,eligible = input$eligibleGroups,method = input$membershipMethod, ID = input$sampleID) %>%
                                       dplyr::mutate_at(dplyr::vars(ID), as.character) %>%
                                       dplyr::left_join(rvals$selectedData %>% dplyr::select(rowid,tidyselect::all_of(input$sampleID)) %>% dplyr::mutate_at(dplyr::vars(rowid), as.character),by = dplyr::join_by("ID" == !!input$sampleID)),error = function(e){
                                         mynotification(paste("error calculating membership:",e),type = "error")
                                         return(NULL)
                                       })
    if(!is.null(rvals$membershipProbs)){
      mynotification("completed calculation")
    }
  })

  output$membershipTbl = DT::renderDataTable({
    req(rvals$membershipProbs)
    if(is.null(rvals$membershipTbl_state_length)){
      rvals$membershipTbl_state_length = 25
    }
    DT::datatable(
      rvals$membershipProbs,
      filter = "top",
      rownames = F,
      selection = 'multiple',
      style = 'bootstrap',
      options = list(
        pageLength = rvals$membershipTbl_state_length,
        lengthMenu = c(10,25,50,100, 500,1000),
        columnDefs = list(list(visible = F, targets = "rowid"))
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
    quietly(label = "updating group",{
      if(!is.null(input$membershipTbl_state$length)){
        rvals$membershipTbl_state_length = input$membershipTbl_state$length
      } else {
        rvals$membershipTbl_state_length = 25
      }
      rowid = rvals$membershipProbs$rowid[input$membershipTbl_rows_selected]
      replaceCell(rowid = rowid,col = rvals$attrGroups,value = rvals$gNewValue, rvals = rvals, con = con, credentials = credentials, input = input, output = output, session = session)
      rvals$gNewValue = NULL
    })
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  })

  observeEvent(input$gNewGroup,{
    if(stringr::str_detect(input$gNewGroup,"[a-zA-z]|[0-9]")){
      shinyjs::enable("gChangeGroup")
    } else {
      shinyjs::disable("gChangeGroup")
    }
  })

}
