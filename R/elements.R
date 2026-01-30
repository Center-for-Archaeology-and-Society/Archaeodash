#' Load Elements from CSV
#'
#' This function loads the \code{elements.csv} file included with the
#' \code{ArchaeoDash} package and returns it as a data frame.
#'
#' @details
#' The function uses \code{system.file()} to locate the \code{elements.csv}
#' file in the \code{app} directory of the installed package.
#'
#' @return A data frame containing the elements defined in
#'   \code{elements.csv}.
#'
#' @examples
#' \dontrun{
#' elements <- load_elements()
#' head(elements)
#' }
#'
#' @export
load_elements <- function() {
    elements <- read.csv(
        system.file("app", "elements.csv", package = "ArchaeoDash"),
        stringsAsFactors = FALSE
    )
    return(elements)
}
