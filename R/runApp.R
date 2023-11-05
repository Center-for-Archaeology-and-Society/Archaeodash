#' Run Application
#'
#' @return shiny app
#' @export
#'
#' @examples
#' runArchaeoDash()
runArchaeoDash = function(){
  appDir <- system.file("app", package = "ArchaeoDash")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ArchaeoDash`.", call. = FALSE)
  }
  warning(paste("app directory is ",appDir))
  shiny::runApp(appDir, display.mode = "normal")
}

