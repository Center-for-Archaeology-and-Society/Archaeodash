
#' UI elements for group membership tab
#'
#' @return
#' @export
#'
#' @examples
groupTab = function(){
  tabPanel(title = "Group Membership",
           sidebarLayout(sidebarPanel(
             chooseDFUI("gm"),
             br(),
             subsetModUI("gm"),
             br(),
             uiOutput("membershipGroupUI"),
             uiOutput("eligibleGroupUI"),
             uiOutput("projectionGroupUI"),
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

  chooseDFServer("gm",rvals)

  subsetModServer("gm",rvals)

  ##### UI Outputs for membership groups ####

  output$membershipGroupUI = renderUI({
    req(rvals$df[[input$`gm-selectedDF`]]$attrData)
    selectInput("membershipGroup","Choose Group Column",choices = colnames(rvals$df[[input$`gm-selectedDF`]]$attrData)[sapply(rvals$df[[input$`gm-selectedDF`]]$attrData, is.factor)])
  })

  output$eligibleGroupUI = renderUI({
    req(input$membershipGroup %in% names(rvals$df[[input$`gm-selectedDF`]]$attrData))
    eligible = getEligible(elements = rvals$df[[input$`gm-selectedDF`]]$chemicalData, attrs = rvals$df[[input$`gm-selectedDF`]]$attrData, group = input$membershipGroup)
    selectInput("eligibleGroups","Choose Eligible Groups",choices = eligible, multiple = T, selected = eligible)
  })

  output$projectionGroupUI = renderUI({
    req(input$eligibleGroups)
    req(input$membershipGroup)
    choices = rvals$df[[input$`gm-selectedDF`]]$attrData %>% dplyr::distinct(!!as.name(input$membershipGroup)) %>%
      dplyr::pull() %>%
      sort()
    selectInput("projectionGroups","Choose Groups to Project Against",choices = choices, multiple = T, selected = choices)
  })

  output$sampleIDUI = renderUI({
    req(rvals$df[[input$`gm-selectedDF`]]$attrData)
    selectInput("sampleID","Choose sample ID Column",choices = names(rvals$df[[input$`gm-selectedDF`]]$attrData))
  })

  output$membershipGroupChoiceUI = renderUI({
    req(input$eligibleGroups)
    req(input$membershipGroup)
    choices = rvals$df[[input$`gm-selectedDF`]]$attrData %>% dplyr::distinct(!!as.name(input$membershipGroup)) %>%
      dplyr::pull() %>%
      sort()
    selectInput("membershipGroupChoice","Choose Group to View",choices = choices)
  })

  observeEvent(input$membershipRun,{
    req(input$membershipGroup)
    showNotification("calculating membership")
    grps = c(input$eligibleGroups,input$projectionGroups)
    indx = which(rvals$df[[input$`gm-selectedDF`]]$attrData[[input$membershipGroup]] %in% grps)
    elements = rvals$df[[input$`gm-selectedDF`]]$chemicalData[indx,]
    attrs = rvals$df[[input$`gm-selectedDF`]]$attrData[indx,]
    rvals$membershipProbs = try(group.mem.probs(elements = elements,assigned = attrs[[input$membershipGroup]],method = input$membershipMethod, ID = attrs[[input$sampleID]]))
    if(inherits(rvals$membershipProbs,"try-error")) showNotification(paste("Error"),rvals$membershipProbs[[1]])
    showNotification("completed")
  })

  output$membershipTbl = DT::renderDataTable({
    DT::datatable(rvals$membershipProbs[[input$membershipGroupChoice]] %>%
      tibble::as_tibble(),rownames = F,selection = 'multiple', style = 'bootstrap')
  })
}
