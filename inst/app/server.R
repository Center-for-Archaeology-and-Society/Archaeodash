#' server.R
library(ArchaeoDash)
library(shiny)

shinyServer(function(input, output, session) {

  ####  create reactive values  ####
  # rvals = reactiveValues(importedData = tibble::tibble(),
                         # selectedData = tibble::tibble())

  ## for testing
  rvals <<- reactiveValues(importedData = tibble::tibble(),
  selectedData = tibble::tibble()); showNotification("warning: global variable is only for testing")
  input <<- input

  #### Import data ####
  dataInputServer(input,output,session,rvals)

  ####  Explore ####
  exploreServer(input,output,session,rvals)

  ####   Ordination   ####
  ordinationServer(input,output,session,rvals)

  ####   Cluster  ####
  clusterServer(input,output,session,rvals)

  ####   Group Membership  ####
  groupServer(input,output,session,rvals)

  ####   Euclidean Distance  ####
  euclideanDistanceSrvr(input,output,session,rvals)

  ####   Visualize & Assign  ####
  visualizeAssignServer(input,output,session,rvals)

  ####   Save & Export  ####
  saveExportServer(input,output,session,rvals)

  #### subsetDataServer ####
  # subsetDataServer(input,output,session,rvals) # no longer used?

}) # end server
