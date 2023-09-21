
#' UI elements for group membership tab
#'
#' @return
#' @export
#'
#' @examples
groupTab = function(){
  tabPanel(title = "Group Membership",
           sidebarLayout(sidebarPanel(
             uiOutput("eligibleGroupUI"),
             uiOutput("sampleIDUI"),
             selectInput("membershipMethod","Select method",choices = c("Hotellings T2"="Hotellings","Mahalanobis distances"="Mahalanobis")),
             actionButton("membershipRun","Calculate"),
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
                               column(4,actionButton("gChangeGroup","Change Group Assignment")),
                               column(4, offset = 2,
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
#' @param input
#' @param output
#' @param session
#' @param rvals
#'
#' @return
#' @export
#'
#' @examples
groupServer = function(input,output,session,rvals){

  ##### UI Outputs for membership groups ####

  output$grpSizeUI = renderUI({
    req(rvals$selectedData)
    tbl = table(rvals$selectedData[[rvals$attrGroups]]) %>%
      as.data.frame() %>%
      setNames(c("Group","Count"))
    DT::renderDataTable(tbl)
  })

  output$eligibleGroupUI = renderUI({
    req(rvals$selectedData)
    eligible = getEligible(rvals$selectedData, chem = rvals$chem, group = rvals$attrGroups)
    selectInput("eligibleGroups","Choose Eligible Groups",choices = eligible, multiple = T, selected = eligible)
  })

  output$sampleIDUI = renderUI({
    req(rvals$selectedData)
    selectInput("sampleID","Choose sample ID Column",choices = names(rvals$selectedData[,rvals$attrs]))
  })

  observeEvent(input$membershipRun,{
    req(rvals$attrGroups)
    showNotification("calculating membership")
    rvals$membershipProbs = try(group.mem.probs(data = rvals$selectedData,chem = rvals$chem, group = rvals$attrGroups,eligible = input$eligibleGroups,method = input$membershipMethod, ID = input$sampleID))
    if(inherits(rvals$membershipProbs,"try-error"))
      showNotification(paste("Error"),rvals$membershipProbs[[1]])
    showNotification("completed Calculation")
  })

  output$membershipTbl = DT::renderDataTable({
    req(rvals$membershipProbs)
    DT::datatable(rvals$membershipProbs,filter = "top",rownames = F,selection = 'multiple', style = 'bootstrap')
  })

  observeEvent(input$gChangeGroup, {
    quietly({
    selRows = input$membershipTbl_rows_selected
    new = rvals$selectedData %>%
      dplyr::slice(selRows) %>%
      dplyr::mutate(!!as.name(rvals$attrGroups) := input$gNewGroup)
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
