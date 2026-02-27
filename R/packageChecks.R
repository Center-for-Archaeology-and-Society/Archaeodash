missing_required_packages <- function(pkgs) {
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}

format_missing_packages_message <- function(feature, missing_pkgs) {
  paste0(
    feature,
    " requires missing package(s): ",
    paste(missing_pkgs, collapse = ", "),
    ". Please install the missing package(s) and restart the app."
  )
}

app_require_packages <- function(pkgs, feature = "This feature", notify = TRUE) {
  missing_pkgs <- missing_required_packages(pkgs)
  if (length(missing_pkgs) == 0) {
    return(TRUE)
  }

  msg <- format_missing_packages_message(feature, missing_pkgs)
  if (isTRUE(notify)) {
    if (exists("mynotification", mode = "function")) {
      try(mynotification(msg, type = "error"), silent = TRUE)
    } else {
      warning(msg, call. = FALSE)
    }
  }
  FALSE
}
