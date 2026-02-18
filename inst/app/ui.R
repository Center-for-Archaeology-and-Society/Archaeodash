# ui for Compositional Analysis Dashboard - Compositions


library(ArchaeoDash)
library(shiny)
library(bslib)

shinyUI(
  bslib::page_fluid(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = paste0("app.js?v=", as.integer(Sys.time())))
    ),
    theme = bs_theme(bootswatch = "sandstone"),
    windowTitle = "ArchaeoDash - A Dashboard for Archaeological Compositional Analysis",
    shinyjs::useShinyjs(),
    id = "page",
    fluidRow(
      column(
        12,
        tags$button(
          id = "toggleSidebar",
          type = "button",
          class = "btn btn-default sidebar-toggle-btn",
          `data-collapsed` = "false",
          "Hide Side Panel"
        )
      )
    ),
    fluidRow(
      column(3,
             id = "sidePanelCol",
             div(
               class = 'mysidecol',
               actionButton('loginUI','login'),
               tags$div(
                 class = "theme-toggle-wrap",
                 selectInput(
                   inputId = "themeSelect",
                   label = "Theme",
                   choices = c(
                     "Simple" = "simple",
                     "Light" = "light",
                     "Dark" = "dark"
                   ),
                   selected = "light",
                   width = "100%",
                   selectize = FALSE
                 )
               ),
               shinyjs::hidden(actionButton('logoutUI','logout')),
               uiOutput("userMessage"),
               br(),
               hr(),
               br(),
               h2("Data Manager"),
               datainputTab(),
               saveexportTab(),
               br()
             )
      ),
      column(9,
             id = "mainPanelCol",
             div(
               class = "mymainpanel",
               navbarPage(
                 title = "ArchaeoDash",
                 id = "nav",
                 homeTab(),
                 exploreTab(),
                 visualizeassignTab(),
                 ordinationTab(),
                 clusterTab(),
                 groupTab(),
                 euclideanDistanceTab(),
                 infoTabUI()
               ) # end navbar
             ) # end div
      ) # end column
    ), # end row
    tags$div(
      id = "cookieBanner",
      class = "cookie-banner",
      style = "display:none;",
      tags$div(
        class = "cookie-banner-content",
        tags$span(
          "We use cookies to keep you logged in for up to 30 days and improve app usability."
        ),
        tags$a(href = "#", id = "privacyPolicyLink", "Privacy Policy"),
        tags$button(id = "acceptCookies", type = "button", class = "btn btn-primary btn-sm", "Accept"),
        tags$button(id = "declineCookies", type = "button", class = "btn btn-default btn-sm", "Decline")
      )
    )
  ) # end page
) # end UI
