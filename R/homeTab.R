
#' UI elements for the Home tab
#'
#' The home tab provides an introduction and basic overview of functionality
# In lieu of directing the user to a self-contained Help section, the
# Home tab will provide links to resources for assistance. These will
# include a blog with posts regarding functionality and updates, contact
# info for developers, and a link to a Google group or similar forum
# for Q+A's as well as FAQ's
#'
#' @return UI
#' @export
#'
#' @examples
#' homeTab()
homeTab = function(){tabPanel(title = "Home", icon = icon("home"),


         div(id = "home",
             fluidRow(column(6,
             p(class = "lead", "Welcome to", strong("ArchaeoDash"),"- ", "A user friendly web application for the statistical analysis and visualization of geochemical compositional data of archaeological materials. The application is currently in beta testing but the app is functional and users are welcome to use it for their data. Please address any problems, concerns, or requests to ",HTML('<a src = "mailto:rbischoff@asu.edu">rbischoff@asu.edu</a>'))))

# Once a name and overall functionality is decided upon, this area can be populated with a basic overview
# of the shiny app and resources to learn more

             )
         )
}
