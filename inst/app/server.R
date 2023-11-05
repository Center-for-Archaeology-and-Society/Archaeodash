#' server.R
library(ArchaeoDash)
library(shiny)

shinyServer(function(input, output, session) {

  ####  create reactive values  ####
  rvals = reactiveValues(importedData = tibble::tibble(),
                         selectedData = tibble::tibble())

  ## for testing
  # rvals <<- reactiveValues(importedData = tibble::tibble(),
  # selectedData = tibble::tibble()); showNotification("warning: global variable is only for testing")
  # input <<- input

  #### Import data ####
  quietly(label = "dataInputServer",dataInputServer(input,output,session,rvals))

  ####  Explore ####
  quietly(label = "exploreServer",exploreServer(input,output,session,rvals))

  ####   Ordination   ####
  quietly(label = "ordinationServer",ordinationServer(input,output,session,rvals))

  ####   Cluster  ####
  quietly(label = "clusterServer",clusterServer(input,output,session,rvals))

  ####   Group Membership  ####
  quietly(label = "groupServer",groupServer(input,output,session,rvals))

  ####   Euclidean Distance  ####
  quietly(label = "euclideanDistanceSrvr",euclideanDistanceSrvr(input,output,session,rvals))

  ####   Visualize & Assign  ####
  quietly(label = "visualizeAssignServer",visualizeAssignServer(input,output,session,rvals))

  ####   Save & Export  ####
  quietly(label = "saveExportServer",saveExportServer(input,output,session,rvals))

  #### subsetDataServer ####
  # subsetDataServer(input,output,session,rvals) # no longer used?

}) # end server
