# ui for Compositional Analysis Dashboard - Compositions

# <<Input details on public use license here -- MIT or GNU or??>>

# Need to think of a better name "TACA" is a placeholder

# Potential options:
# TACA (Tools for Archaeological Compositional Analysis)
# ACAD (Archaeological Compositional Analysis Dashboard)
# ArchaeoDash
# ArchCompAnalysis
# GeocompAnalysis
# AECA (Archaeological elemental compositonal analysis)
# Comp_Dash
library(ArchaeoDash)
library(shiny)
library(bslib)

shinyUI(
  bslib::page_sidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    theme = shinythemes::shinytheme("sandstone"),
    windowTitle = "ArchaeoDash - A Dashboard for Archaeological Compositional Analysis",
    shinyjs::useShinyjs(),
    fluid = TRUE,
    id = "nav",
    sidebar = bslib::sidebar(
      class = "mysidebar",
      h1("Data Manager"),
      open = T,
      bg = "#9dd4b9",
      datainputTab(),
      saveexportTab()
    ),
    div(
      class = "mymainpanel",
      navbarPage(
        title = "ArchaeoDash",
        homeTab(),
        imputetransformTab(),
        ordinationTab(),
        clusterTab(),
        groupTab(),
        visualizeassignTab()
      )
    )
  )
)
