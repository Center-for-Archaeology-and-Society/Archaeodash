

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
homeTab = function() {
  tabPanel(title = "Home",
           icon = icon("home"),


           div(id = "home",
               fluidRow(column(
                 6,
                 p(
                   class = "lead",
                   "Welcome to",
                   strong("ArchaeoDash"),
                   "- ",
                   "A user friendly web application for the statistical analysis and visualization of geochemical compositional data of archaeological materials. The application is currently in beta testing but the app is functional and users are welcome to use it for their data"),
                 p(
                   class = "lead",
                   "Online version hosted at ",
                   shiny::a(
                     href="https://collectiveaccess.rc.asu.edu/app/Archaeodash/",
                     target = "0",
                     "collectiveaccess.rc.asu.edu/app/Archaeodash/"
                            ),
                   " and supported by the Center for Archaeology and Society at Arizona State University."
                 ),
                 p(
                   class = "lead",
                   "Please submit any bugs or feature requests to the Github issues page ",
                   shiny::a(
                     href="https://github.com/Center-for-Archaeology-and-Society/Archaeodash/issues",
                     target = "0",
                     "here"
                   )
                 ),
                 p(
                   glue::glue(
                     "{system.file(\"DESCRIPTION\",package = \"ArchaeoDash\") %>% readLines() %>% .[which(stringr::str_detect(.,\"Version\"))]}"
                   )
                 )
               ))))
}
