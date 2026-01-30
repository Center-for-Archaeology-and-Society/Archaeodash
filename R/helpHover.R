#' Add a Help Icon with Hover Text
#'
#' This function creates a label with an icon (default is a question mark) that shows
#' a tooltip when hovered over. It is useful for adding inline help text in Shiny apps.
#'
#' @param tag Character or tag object. The main label or content to display.
#' @param text Character. The hover text that appears when the user hovers over the icon.
#' @param icon_class Character. CSS class for the icon. Defaults to
#'   \code{"fa fa-question-circle"} (a Font Awesome question mark).
#'
#' @return An HTML span tag containing the label and help icon, suitable for use in
#'   Shiny UI definitions.
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'     help_icon("Input Value", "Enter a numeric value here. It must be >= 0."),
#'     numericInput("num", NULL, 1, min = 0)
#' )
#'
#' server <- function(input, output, session) {}
#'
#' if (interactive()) shinyApp(ui, server)
#'
#' @export
help_icon <- function(tag, text, icon_class = "fa fa-question-circle") {
    tags$span(
        tag,
        tags$i(
            class = icon_class,
            title = text,
            style = "margin-left:5px; cursor: help;"
        )
    )
}
