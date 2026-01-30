#' Quiet Warning Function
#'
#' @param .expr expression
#'
#' @return results of function
#' @export
#'
#' @examples
#' f <- sum(1 + "a")
#' quietly(f())
quietly <- function(.expr, label = NULL) {
  result <- function(...) {
    last_warning <- NULL
    res <- tryCatch(
      {
        result <- withCallingHandlers(
          expr = eval(substitute(.expr), envir = parent.frame()),
          warning = function(w) {
            last_warning <<- w
            invokeRestart("muffleWarning")
          }
        )
        list(result = result, error = NULL, warnings = last_warning)
      },
      error = function(e) {
        list(result = NULL, error = e, warnings = NULL)
      }
    )

    if (!is.null(res$error)) {
      mynotification(paste(label, res$error$message), duration = 10, type = "error")
      return(NULL)
    }

    if (!is.null(res$warnings)) {
      mynotification(paste(label, res$warnings$message), duration = 10, type = "warning")
    }

    return(res$result)
  }
  result()
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
mynotification <- function(message, type = c("default", "message", "warning", "error", "success"), duration = 5, closeButton = FALSE) {
  type <- match.arg(type)
  if (type %in% c("warning", "error")) {
    warning(message)
  } else {
    message(message)
  }
  try(showNotification(message, type = type, duration = duration, closeButton = closeButton), silent = T)
}
