#' infoTabUI
#'
#' @return UI
#' @export
#'
#' @examples
#' infoTabUI()
infoTabUI = function() {
  navbarMenu("Info",
             tabPanel("Help", value = "help",
                      icon = icon("info"),
                      tags$iframe(
                        src = "help.html",
                        style = "width:100%; height:80vh; border:none;"
                      )
             ),
             tabPanel("Terms & Conditions", value = "terms",
                      icon = icon("info"),
                      includeMarkdown("www/terms.md")
             ),
             tabPanel("Privacy Policy", value = "privacy",
                      icon = icon("info"),
                      includeMarkdown("www/privacy.md")
             )
  )
}
