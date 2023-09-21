#' Quiet Warning Function
#'
#' @param .expr expression
#'
#' @return results of function
#'
#' @examples
#' f = sum(1 + 'a')
#' quietly(f())
quietly <- function(.expr) {
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
      showNotification(res$error$message, duration = 10, type = "error")
      return(NULL)
    }

    if (!is.null(res$warnings) && length(res$warnings) > 0) {
      lapply(unique(res$warnings), function(w) {
        showNotification(w$message, duration = 10, type = "warning")
      })
    }

    return(res$result)
  }
  return(result(.expr))
}
