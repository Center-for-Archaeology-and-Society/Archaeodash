#' Whether verbose app logging is enabled
#'
#' @return logical scalar
#' @export
app_is_verbose <- function() {
  isTRUE(getOption("archaeodash.verbose", FALSE))
}

#' Conditional app logger
#'
#' @param ... values forwarded to base::message when verbose mode is enabled
#'
#' @return invisible NULL
#' @export
app_log <- function(...) {
  if (!app_is_verbose()) return(invisible(NULL))
  base::message(...)
  invisible(NULL)
}
