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
                      includeHTML("www/help.html")
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
