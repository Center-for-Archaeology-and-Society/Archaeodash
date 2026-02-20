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
                      tags$div(
                        style = "margin-bottom: 10px;",
                        tags$a(
                          href = "help.html",
                          target = "_blank",
                          rel = "noopener noreferrer",
                          "Open Help in a new tab"
                        ),
                        tags$span(" | "),
                        tags$a(
                          href = "#",
                          onclick = "document.getElementById('help_iframe').src='help.html'; return false;",
                          "Reload Help"
                        )
                      ),
                      tags$iframe(
                        id = "help_iframe",
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
