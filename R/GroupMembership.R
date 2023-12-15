
#' UI elements for group membership tab
#'
#' @return UI
#' @export
#'
#' @examples
#' groupTab()
groupTab = function(){
  tabPanel(title = "Group Membership",
           sidebarLayout(sidebarPanel(
             uiOutput("eligibleGroupUI"),
             uiOutput("sampleIDUI"),
             selectInput("membershipMethod","Select method",choices = c("Hotellings T2"="Hotellings","Mahalanobis distances"="Mahalanobis")),
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
groupServer = function(input,output,session,rvals){

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
    if(!is.null(rvals$attrs)){
      choices = tryCatch(names(rvals$selectedData[,rvals$attrs]),error = function(e) return(NULL))
    } else {
      choices = NULL
    }
    if(is.null(rvals$sampleID)){
      selected = choices[1]
      if(selected == "rowid") selected = choices[2]
    } else {
      selected = rvals$sampleID
    }
    selectInput("sampleID","Choose sample ID Column",choices = choices,selected = selected)
  })

  observeEvent(input$membershipRun,{
    req(rvals$attrGroups)
    showNotification("calculating membership")
    rvals$eligibleGroups = input$eligibleGroups
    rvals$sampleID = input$sampleID
    rvals$membershipProbs = try(group.mem.probs(data = rvals$selectedData,chem = rvals$chem, group = rvals$attrGroups,eligible = input$eligibleGroups,method = input$membershipMethod, ID = input$sampleID))
    if(inherits(rvals$membershipProbs,"try-error"))
      showNotification(paste("Error"),rvals$membershipProbs[[1]])
    showNotification("completed calculation")
  })

  output$membershipTbl = DT::renderDataTable({
    req(rvals$membershipProbs)
    if(is.null(rvals$membershipTbl_state_length)){
      rvals$membershipTbl_state_length = 25
    }
    DT::datatable(rvals$membershipProbs,filter = "top",rownames = F,selection = 'multiple', style = 'bootstrap',options = list(
      pageLength = rvals$membershipTbl_state_length,
                                                                                                                               lengthMenu = c(10,25,50,100, 500,1000)))
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
      value = rvals$gNewValue
      rvals$gNewValue = NULL
      selRows = input$membershipTbl_rows_selected
      if(length(selRows)!= length(value)){
        value = value[1]
      }
      if(!is.null(input$membershipTbl_state$length)){
        rvals$membershipTbl_state_length = input$membershipTbl_state$length
      } else {
        rvals$membershipTbl_state_length = 25
      }
      new = rvals$selectedData %>%
        dplyr::slice(selRows) %>%
        dplyr::mutate(!!as.name(rvals$attrGroups) := value)
      old = rvals$selectedData %>%
        dplyr::slice(-selRows)
      rvals$selectedData = dplyr::bind_rows(new, old) %>%
        dplyr::arrange(rowid) %>%
        dplyr::mutate_at(dplyr::vars(tidyselect::all_of(rvals$attrGroups)),factor)
      rvals$membershipProbs[['GroupVal']] = rvals$selectedData[[rvals$attrGroups]]
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
