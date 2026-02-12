#' server.R
library(ArchaeoDash)
library(shiny)
library(profvis)

# options(shiny.reactlog = TRUE)

shinyServer(function(input, output, session) {
  ###  create reactive values  ####
  rvals <- reactiveValues()
  db <- reactiveValues(
    datasets = data.frame(
      sample_uuid = character(),
      dataset_uuid = character(),
      dataset_label = character(),
      stringsAsFactors = FALSE
    ),
    labels = data.frame(
      sample_uuid = character(),
      label_type = character(),
      label = character(),
      stringsAsFactors = FALSE
    ),
    measurements = data.frame(
      sample_uuid = character(),
      measurement_label = character(),
      value = numeric(),
      measurement_type = character(),
      stringsAsFactors = FALSE
    )
  )

  #### Import data ####

  quietly(label = "dataInputServer", dataInputServer(input, output, session, rvals, db))

  ####  Explore ####
  quietly(label = "exploreServer", exploreServer(input, output, session, rvals))

  ####   Ordination   ####
  quietly(label = "ordinationServer", ordinationServer(input, output, session, rval, dbs))

  ####   Cluster  ####
  quietly(label = "clusterServer", clusterServer(input, output, session, rvals, db))

  ####   Group Membership  ####
  quietly(label = "groupServer", groupServer(input, output, session, rvals, db))

  ####   Euclidean Distance  ####
  quietly(label = "euclideanDistanceSrvr", euclideanDistanceSrvr(input, output, session, rvals, db))

  ####   Visualize & Assign  ####
  quietly(label = "visualizeAssignServer", visualizeAssignServer(input, output, session, rvals, db))

  ####   Save & Export  ####
  quietly(label = "saveExportServer", saveExportServer(input, output, session, rvals, db))
}) # end server
