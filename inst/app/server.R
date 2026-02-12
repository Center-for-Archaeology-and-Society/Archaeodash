#' server.R
library(ArchaeoDash)
library(shiny)
library(profvis)

# options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 100 * 1024^2)

shinyServer(function(input, output, session) {

  ###  create reactive values  ####
  rvals = reactiveValues(importedData = tibble::tibble(),
  selectedData = tibble::tibble())
  credentials = reactiveValues()
  # for testing
  # rvals <<- reactiveValues(importedData = tibble::tibble(),
  # selectedData = tibble::tibble()); showNotification("warning: global variable is only for testing", type = "warning")
  # input <<- input; showNotification("warning: global variable is only for testing", type = "warning")
  # session <<- session; showNotification("warning: global variable is only for testing", type = "warning")
  # credentials <<- reactiveValues(); showNotification("warning: global variable is only for testing", type = "warning")

  con = tryCatch(connect(),error = function(e) {
    mynotification("unable to connect to database", type = "warning")
    NULL
    })

  loginUI(input = input)

  loginServer(con, input = input, output = output, session = session, credentials = credentials)

  observeEvent(credentials$res$username, {
    if(!is.null(credentials$res$username) && !is.na(credentials$res$username)){
      output$userMessage = renderUI({
        renderText(paste("Logged in as", credentials$res$username))
      })
      shinyjs::hide(id = "loginUI")
      shinyjs::show(id = "logoutUI")
    } else {
      output$userMessage = renderUI({
        NULL
      })
      shinyjs::show(id = "loginUI")
      shinyjs::hide(id = "logoutUI")
    }
  })

  observeEvent(input$logoutUI,{
    credentials$res = tibble::tibble(username = NA)
  })

  #### Import data ####

  quietly(label = "dataInputServer",dataInputServer(input,output,session,rvals,con, credentials))

  ####  Explore ####
  quietly(label = "exploreServer",exploreServer(input,output,session,rvals,con,credentials))

  ####   Ordination   ####
  quietly(label = "ordinationServer",ordinationServer(input,output,session,rvals))

  ####   Cluster  ####
  quietly(label = "clusterServer",clusterServer(input,output,session,rvals, credentials, con))

  ####   Group Membership  ####
  quietly(label = "groupServer",groupServer(input,output,session,rvals, credentials, con))

  ####   Euclidean Distance  ####
  quietly(label = "euclideanDistanceSrvr",euclideanDistanceSrvr(input,output,session,rvals, credentials, con))

  ####   Visualize & Assign  ####
  quietly(label = "visualizeAssignServer",visualizeAssignServer(input,output,session,rvals, credentials, con))

  ####   Save & Export  ####
  quietly(label = "saveExportServer",saveExportServer(input,output,session,rvals))

  #### subsetDataServer ####
  subsetDataServer(input,output,session,rvals) # no longer used?

}) # end server
