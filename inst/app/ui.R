library(ArchaeoDash)
library(shiny)
library(bslib)

shinyUI(
  page_fluid(
    theme = bs_theme(bootswatch = "sandstone"),
    windowTitle = "ArchaeoDash - A Dashboard for Archaeological Compositional Analysis",
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    id = "page",
    page_navbar(
      title = "ArchaeoDash",
      id = "navbar",
      sidebar = sidebar(
        id = "mysidecol",
        width = 300,
        open = "closed",
        actionButton("loginUI", "login"),
        shinyjs::hidden(actionButton("logoutUI", "logout")),
        uiOutput("userMessage"),
        br(), hr(), br(),
        h2("Data Manager"),
        datainputTab(),
        saveexportTab(),
        br()
      ),
      homeTab(),
      exploreTab(),
      visualizeassignTab(),
      ordinationTab(),
      clusterTab(),
      groupTab(),
      euclideanDistanceTab(),
      infoTabUI()
    )
  )
)
