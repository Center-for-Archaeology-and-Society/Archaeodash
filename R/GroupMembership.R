
#' UI elements for group membership tab
#'
#' @return
#' @export
#'
#' @examples
groupTab = function(){
  tabPanel(title = "Group Membership",
           sidebarLayout(sidebarPanel(
             uiOutput("membershipGroupUI"),
             uiOutput("projectionGroupUI"),
             uiOutput("eligibleGroupUI"),
             uiOutput("sampleIDUI"),
             uiOutput("membershipGroupChoiceUI"),
             selectInput("membershipMethod","Select method",choices = c("Hotellings T2"="Hotellings","Mahalanobis distances"="Mahalanobis")),
             actionButton("membershipRun","Calculate"),
           ),
           mainPanel(
             fluidRow(
               column(8,
                      DT::DTOutput('membershipTbl')
               ),
               column(4,
                      fluidRow(actionButton("gAddGroup","Add New Column")),
                      fluidRow(br()),
                      fluidRow(actionButton("gChangeGroup","Change Group Assignment")),
                      fluidRow(textInput("gNewGroup","Enter new group designation")))
             )
           ))
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

  output$eligibleGroupUI = renderUI({
    req(rvals$attrGroups %in% names(rvals$selectedData[,rvals$attrs]))
    eligible = getEligible(elements = rvals$selectedData[,rvals$chem], attrs = rvals$selectedData[,rvals$attrs], group = rvals$attrGroups)
    selectInput("eligibleGroups","Choose Eligible Groups",choices = eligible, multiple = T, selected = eligible)
  })

  output$sampleIDUI = renderUI({
    req(rvals$selectedData[,rvals$attrs])
    selectInput("sampleID","Choose sample ID Column",choices = names(rvals$selectedData[,rvals$attrs]))
  })

  output$membershipGroupChoiceUI = renderUI({
    req(input$eligibleGroups)
    req(rvals$attrGroups)
    choices = rvals$selectedData[,rvals$attrs] %>% dplyr::distinct(!!as.name(rvals$attrGroups)) %>%
      dplyr::pull() %>%
      sort()
    selectInput("membershipGroupChoice","Choose Group to View",choices = choices)
  })

  observeEvent(input$membershipRun,{
    req(rvals$attrGroups)
    showNotification("calculating membership")
    grps = c(input$eligibleGroups,input$projectionGroups)
    indx = which(rvals$selectedData[,rvals$attrs][[rvals$attrGroups]] %in% grps)
    elements = rvals$selectedData[,rvals$chem][indx,]
    attrs = rvals$selectedData[,rvals$attrs][indx,]
    rvals$membershipProbs = try(group.mem.probs(elements = elements,assigned = attrs[[rvals$attrGroups]],valid = input$projectionGroups,method = input$membershipMethod, ID = attrs[[input$sampleID]]))
    if(inherits(rvals$membershipProbs,"try-error")) showNotification(paste("Error"),rvals$membershipProbs[[1]])
    showNotification("completed")
  })

  output$membershipTbl = DT::renderDataTable({
    DT::datatable(rvals$membershipProbs,filter = "top",rownames = F,selection = 'multiple', style = 'bootstrap')
  })


  observeEvent(input$gChangeGroup, {
    selRows = input$membershipTbl_rows_selected
    new = rvals$selectedData[,rvals$attrs] %>%
      dplyr::slice(selRows) %>%
      dplyr::mutate(!!as.name(input$Code) := input$gNewGroup)
    old = rvals$selectedData[,rvals$attrs] %>%
      dplyr::slice(-selRows)
    rvals$selectedData[,rvals$attrs] = dplyr::bind_rows(new, old) %>%
      dplyr::arrange(rowid) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::all_of(input$Code)),factor)

  })

  observeEvent(input$gNewGroup,{
    if(stringr::str_detect(input$gNewGroup,"[a-zA-z]|[0-9]")){
      shinyjs::enable("gChangeGroup")
    } else {
      shinyjs::disable("gChangeGroup")
    }
  })

}
