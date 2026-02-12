# ui for Compositional Analysis Dashboard - Compositions

library(ArchaeoDash)
library(shiny)
library(bslib)

shinyUI(
  bslib::page_fluid(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    theme = bs_theme(bootswatch = "sandstone"),
    windowTitle = "ArchaeoDash - A Dashboard for Archaeological Compositional Analysis",
    shinyjs::useShinyjs(),
    id = "page",
    div(
      class = "mymainpanel",
      navbarPage(
        title = "ArchaeoDash",
        id = "nav",
        tabPanel(
          "Data & Login",
          div(
            class = "dataInput",
            h2("Data Manager"),
            datainputTab(),
            saveexportTab(),
            br()
          )
        ),
        homeTab(),
        exploreTab(),
        ordinationTab(),
        clusterTab(),
        groupTab(),
        euclideanDistanceTab(),
        visualizeassignTab(),
        infoTabUI()
      ) # end navbarPage
    ) # end div
  ) # end page_fluid
) # end UI
