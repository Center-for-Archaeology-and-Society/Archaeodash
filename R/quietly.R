#' Quiet Warning Function
#'
#' @param .expr expression
#'
#' @return results of function
#' @export
#'
#' @examples
#' f = sum(1 + 'a')
#' quietly(f())
quietly <- function(.expr,label = NULL) {
  result = function(...) {
    res <- tryCatch(
      {
        result <- eval(expr = substitute(.expr), envir = parent.frame())
        list(result = result, error = NULL, warnings = NULL)
      },
      error = function(e) {
        list(result = NULL, error = e, warnings = NULL)
      },
      warning = function(w) {
        list(result = NULL, error = NULL, warnings = w)
      }
    )

    if (!is.null(res$error)) {
      mynotification(paste(label,res$error$message), duration = 10, type = "error")
      return(NULL)
    }

    if (!is.null(res$warnings) && length(res$warnings) > 0) {
      lapply(unique(res$warnings), function(w) {
        mynotification(paste(label,w$message), duration = 10, type = "warning")
      })
    }

    return(res$result)
  }
  return(result(.expr))
}

#' custom notification
#'
#' @param message message to display
#' @param type type of message
#'
#' @return notification
#' @export
#'
#' @examples
#' mynotification("hello world")
mynotification <- function(message, type = c("default", "message", "warning", "error", "success"), duration = 5) {
  type <- match.arg(type)
  if(type %in% c("warning", "error")) {
    warning(message)
  } else {
    message(message)
  }
  try(showNotification(message, type = type, duration = duration),silent = T)
}
