

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
           id = "hometab",
           icon = icon("home"),


           div(id = "home",
               fluidRow(column(
                 6,
                 p(
                   class = "lead",
                   "Welcome to",
                   strong("ArchaeoDash"),
                   "- ",
                   "A user friendly web application for the statistical analysis and visualization of geochemical compositional data of archaeological materials. The application is currently in beta testing but the app is functional and users are welcome to use it for their data. Please see the INFO tab menu for help, terms and conditions, and privacy policy."),
                 p(class = "lead","Users may create an account where data will be stored on a server. Users retain all rights to their data and may delete it at any time. Data will not be shared with any third party. Users are welcome to use the app without creating an account but no guarantees are made regarding the persistence of data with or without an account."),
                 p(
                   class = "lead",
                   "Online version hosted at ",
                   shiny::a(
                     href="https://cas.rc.asu.edu/app/Archaeodash/",
                     target = "0",
                     "cas.rc.asu.edu/app/Archaeodash/"
                            ),
                   " and supported by the Center for Archaeology and Society at Arizona State University."
                 ),
                 p(class = "lead","No warranties are made and the app is provided as is under the MIT license. The app is open source and users are welcome to contribute to the code base."),
                 p(
                   class = "lead",
                   "Please submit any bugs or feature requests to the Github issues page ",
                   shiny::a(
                     href="https://github.com/Center-for-Archaeology-and-Society/Archaeodash/issues",
                     target = "0",
                     "here"
                   )
                 ),
                 p(class = "lead",
                   glue::glue(
                     "{system.file(\"DESCRIPTION\",package = \"ArchaeoDash\") %>% readLines() %>% .[which(stringr::str_detect(.,\"Version\"))]}"
                   )
                 )
               ))))
}
