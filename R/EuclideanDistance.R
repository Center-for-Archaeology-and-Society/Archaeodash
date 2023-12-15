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
           sidebarLayout(
             sidebarPanel(
               uiOutput("projectionGroupUI"),
               uiOutput("EDsampleIDUI"),
               radioButtons(
                 inputId = "EDmethod",
                 label = "Project within group?",choices = c(TRUE,FALSE),
                 selected = FALSE
               ),
               sliderInput(
                 "EDlimit",
                 "Number of closest matches to return for each observation",
                 min = 1,
                 max = 100,
                 value = 10
               ),
               actionButton("EDRun", "Calculate", class = "mybtn")
             ),
             mainPanel(
               tabPanel(
                 title = "Euclidean Distance",
                 id = "eDistance",
                 wellPanel(fluidRow(
                   column(4, actionButton(
                     'edAssignMatchGroup','Assign Match Group', class = "mybtn"
                   )),
                   column(
                     4,
                     offset = 2,
                     actionButton("edChangeGroup", "Change Group Assignment", class = "mybtn"),
                     textInput("edNewGroup", "Enter new group designation")
                   )
                 )),
                 br(),
                 DT::DTOutput('EDTbl')
               )
             ) # end main panel
           ) # end sidebar layout
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
euclideanDistanceSrvr = function(input,output,session,rvals) {

  output$projectionGroupUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    if(!is.null(rvals$attrGroups)){
      choices = tryCatch(sort(unique(as.character(rvals$selectedData[[rvals$attrGroups]]))),error = function(e) return(NULL))
    } else {
      choices = NULL
    }

    selectInput(
      "projectionGroup",
      "Select groups to project",
      choices = choices,
      multiple = T
    )
  })

  output$EDsampleIDUI = renderUI({
    req(nrow(rvals$selectedData) > 0)
    quietly(label = "rendering sample ID UI",{
      if(!is.null(rvals$attrs)){
        choices = tryCatch(names(rvals$selectedData[,rvals$attrs]),error = function(e) return(NULL))
      } else {
        choices = NULL
      }
      if("anid" %in% tolower(choices)){
        selected = choices[which(tolower(choices) == "anid")]
      } else {
        selected = choices[which(!choices %in% c("rowid","file"))][1]
      }
      selectInput("edsampleID","Choose sample ID Column",choices = choices, selected = selected)
    })
  })

  observeEvent(input$EDRun,{
    quietly(label = "running Euclidean Distance",{
      mynotification("calculating Euclidean Distances")
      rvals$edistance = calcEDistance(data = rvals$selectedData,projection = input$projectionGroup,id = input$edsampleID,attrGroups = rvals$attrGroups,chem = rvals$chem,limit = input$EDlimit, withinGroup = input$EDmethod) %>%
        dplyr::left_join(rvals$selectedData %>% dplyr::select(rowid,tidyselect::all_of(input$edsampleID)),by = dplyr::join_by(!!input$sampleID))
      mynotification("completed calculation")
    })
  })

  output$EDTbl = DT::renderDataTable({
    req(rvals$edistance)
    quietly(label = "rendering Euclidean Distance table",{
      if(is.null(rvals$EDTbl_state_length)){
        rvals$EDTbl_state_length = 25
      }
      DT::datatable(
        rvals$edistance,filter = "top",rownames = F,selection = 'multiple', style = 'bootstrap', options = list(
          pageLength = rvals$EDTbl_state_length,
          lengthMenu = c(10,25,50,100, 500,1000),
          columnDefs = list(list(visible = F, targets = "rowid"))
        )
      )
    })
  })

  edProxy = DT::dataTableProxy('EDTbl')

  observeEvent(input$edAssignMatchGroup,{
    quietly(label = "assigning match group",{
      selRows = input$EDTbl_rows_selected
      rvals$edNewValue = rvals$edistance[[5]][selRows]
    })
  })

  observeEvent(input$edChangeGroup,{
    quietly(label = "assigning new group",{
      rvals$edNewValue = input$edNewGroup
    })
  })

  observeEvent(rvals$edNewValue, {
    quietly(label = "changing group",{
      rowid = rvals$membershipProbs$rowid[input$EDTbl_rows_selected]
      replaceCell(rvals,rowid,rvals$attrGroups,rvals$edNewValue)
      rvals$edNewValue = NULL
      DT::replaceData(edProxy, rvals$edistance, resetPaging = FALSE)
      if(!is.null(input$EDTbl_search)){
        edProxy %>% DT::updateSearch(keywords = list(global = input$EDTbl_search))
      }
      if(!is.null(input$EDTbl_state$length)){
        rvals$EDTbl_state_length = input$EDTbl_state$length
      } else {
        rvals$EDTbl_state_length = 25
      }

    })
    rvals$xvar = tryCatch(input$xvar,error = function(e)return(NULL))
    rvals$xvar2 = tryCatch(input$xvar2,error = function(e)return(NULL))
    rvals$yvar = tryCatch(input$yvar,error = function(e)return(NULL))
    rvals$yvar2 = tryCatch(input$yvar2,error = function(e)return(NULL))
    rvals$data.src = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$Conf = tryCatch(input$data.src,error = function(e)return(NULL))
    rvals$int.set = tryCatch(input$int.set,error = function(e)return(NULL))
  })

  observeEvent(input$edNewGroup,{
    if(stringr::str_detect(input$edNewGroup,"[a-zA-z]|[0-9]")){
      shinyjs::enable("edChangeGroup")
    } else {
      shinyjs::disable("edChangeGroup")
    }
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
  result = NULL
  quietly(label = "calcEDistance",{
    projections = data %>%
      dplyr::filter(!!as.name(attrGroups) %in% projection) %>%
      dplyr::pull(!!as.name(id))
    m = data[,chem] %>% as.matrix()
    rownames(m) = data[[id]]
    d = dist(m,method = "euclidean")
    result = as.data.frame(as.matrix(d)) %>%
      dplyr::select(tidyselect::any_of(projections)) %>%
      tibble::rownames_to_column("match") %>%
      tidyr::pivot_longer(-match, names_to = "observation", values_to = "distance") %>%
      dplyr::filter(match != observation) %>%
      dplyr::group_by(observation) %>%
      dplyr::arrange(distance) %>%
      dplyr::slice_head(n = limit) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::left_join(data %>%
                         dplyr::select(observation = tidyselect::any_of(id), observationGroup = tidyselect::any_of(attrGroups)) %>%
                         dplyr::mutate_all(as.character), by = "observation") %>%
      dplyr::left_join(data %>%
                         dplyr::select(match = tidyselect::any_of(id), matchGroup = tidyselect::any_of(attrGroups)) %>%
                         dplyr::mutate_all(as.character), by = "match") %>%
      dplyr::select(tidyselect::any_of(c('observation','match','distance','observationGroup','matchGroup'))) %>%
      dplyr::arrange(observation,distance) %>%
      dplyr::rename(!!as.name(id) := observation,!!as.name(attrGroups) := observationGroup,!!as.name(paste0(attrGroups,"_match")) := matchGroup) %>%
      dplyr::mutate_at(dplyr::vars(distance),as.numeric) %>%
      # format distance to 4 decimal places
      dplyr::mutate_at(dplyr::vars(distance),sprintf, fmt = "%0.4f")
    if(withinGroup == FALSE){
      result = result %>%
        dplyr::filter(!!as.name(attrGroups) != !!as.name(paste0(attrGroups,"_match")))
    }
  })
  return(result)
}
