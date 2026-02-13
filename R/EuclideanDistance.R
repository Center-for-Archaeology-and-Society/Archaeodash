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
               selectInput(
                 "EDdataset",
                 "Select dataset to use",
                 choices = c("elements", "principal components", "UMAP", "linear discriminants"),
                 selected = "elements"
               ),
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
euclideanDistanceSrvr = function(input,output,session,rvals,credentials, con) {
  get_ed_source_features <- function(df, source) {
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
      choiceLengths = sapply(choices,function(x) length(unique(source_df[[x]])))
      choices = choices[which(choiceLengths == nrow(source_df))]
      if("anid" %in% tolower(choices)){
        selected = choices[which(tolower(choices) == "anid")]
      } else {
        selected = choices[1]
      }
      selectInput("edsampleID","Choose sample ID Column",choices = choices, selected = selected[1])
    })
  })

  observeEvent(input$EDRun,{
    quietly(label = "running Euclidean Distance",{
      source_data <- get_ed_data(if (is.null(input$EDdataset)) "elements" else input$EDdataset, notify = TRUE)
      if (is.null(source_data)) return(invisible(NULL))
      analysis_df <- source_data$df
      feature_cols <- source_data$features
      if (!input$edsampleID %in% names(analysis_df)) {
        mynotification("Selected sample ID column is not available in this dataset source.", type = "error")
        return(invisible(NULL))
      }
      mynotification("calculating Euclidean Distances")
      rvals$edistance = calcEDistance(
        data = analysis_df,
        projection = input$projectionGroup,
        id = input$edsampleID,
        attrGroups = rvals$attrGroups,
        chem = feature_cols,
        limit = input$EDlimit,
        withinGroup = input$EDmethod
      ) %>%
        dplyr::left_join(
          analysis_df %>% dplyr::select(rowid,tidyselect::all_of(input$edsampleID)) %>% dplyr::mutate_all(as.character),
          by = input$edsampleID
        )
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
        rvals$edistance %>%
          dplyr::mutate_at(dplyr::vars(distance),as.numeric) %>%
          dplyr::mutate_at(dplyr::vars(distance),round,1) %>%
          dplyr::arrange(!!as.name(input$edsampleID),distance),filter = "top",rownames = F,selection = 'multiple', style = 'bootstrap', options = list(
          pageLength = rvals$EDTbl_state_length,
          lengthMenu = c(10,25,50,100, 500,1000)
        )
      )
    })
  })

  edProxy = DT::dataTableProxy('EDTbl')

  observeEvent(input$edAssignMatchGroup,{
    quietly(label = "assigning match group",{
      selRows = input$EDTbl_rows_selected
      match_col <- paste0(rvals$attrGroups, "_match")
      if (!match_col %in% names(rvals$edistance)) {
        mynotification("Unable to locate matched-group column in Euclidean Distance results.", type = "error")
        return(invisible(NULL))
      }
      rvals$edNewValue = rvals$edistance[[match_col]][selRows]
    })
  })

  observeEvent(input$edChangeGroup,{
    quietly(label = "assigning new group",{
      rvals$edNewValue = input$edNewGroup
    })
  })

  observeEvent(rvals$edNewValue, {
    quietly(label = "changing group",{
      print("rows selected")
      print(input$EDTbl_rows_selected)
      print(head(rvals$edistance))
      rowid = rvals$edistance$rowid[input$EDTbl_rows_selected]
      print("rowid")
      print(rowid)
      replaceCell(rowid = rowid,col = rvals$attrGroups,value = rvals$edNewValue, rvals = rvals, con = con, credentials = credentials, input = input, output = output, session = session)
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
    result = tibble::as_tibble(as.matrix(d)) %>%
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
