#' Run Application
#'
#' @param verbose logical; if TRUE, enables verbose console logging for debugging.
#'
#' @return shiny app
#' @export
#'
#' @examples
#' runArchaeoDash()
runArchaeoDash = function(verbose = FALSE){
  ensure_shiny_dependency_aliases()
  options(archaeodash.verbose = isTRUE(verbose))
  appDir <- system.file("app", package = "ArchaeoDash")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ArchaeoDash`.", call. = FALSE)
  }
  app_log(paste("app directory is", appDir))
  shiny::runApp(appDir, display.mode = "normal")
}

