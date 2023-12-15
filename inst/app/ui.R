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
    fluid = TRUE,
    id = "nav",
    fluidRow(
      column(3,
             div(
               class = 'mysidecol',
               h1("Data Manager"),
               datainputTab(),
               saveexportTab(),
               br()
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
