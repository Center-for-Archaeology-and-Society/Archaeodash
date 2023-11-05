# ui for Compositional Analysis Dashboard - Compositions


library(ArchaeoDash)
library(shiny)
library(bslib)

shinyUI(
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    theme = shinythemes::shinytheme("sandstone"),
    windowTitle = "ArchaeoDash - A Dashboard for Archaeological Compositional Analysis",
    shinyjs::useShinyjs(),
    fluid = TRUE,
    id = "nav",
    fluidRow(
      column(3,
             div(
               class = 'mysidecol',
               h1("Data Manager"),
               datainputTab(),
               saveexportTab()
             )
      ),
      column(9,
             div(
               class = "mymainpanel",
               navbarPage(
                 title = "ArchaeoDash",
                 homeTab(),
                 exploreTab(),
                 ordinationTab(),
                 clusterTab(),
                 groupTab(),
                 euclideanDistanceTab(),
                 visualizeassignTab()
               ) # end navbar
             ) # end div
      ) # end column
    ) # end row
  ) # end page
) # end UI
