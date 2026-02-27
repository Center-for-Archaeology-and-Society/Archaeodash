dataset_load_timeout_seconds <- function() {
  30L
}

is_dataset_load_timeout_error <- function(err) {
  if (is.null(err)) return(FALSE)
  msg <- tolower(conditionMessage(err))
  grepl("elapsed time limit", msg, fixed = TRUE) || grepl("reached elapsed", msg, fixed = TRUE)
}

with_dataset_load_timeout <- function(expr, timeout_sec = dataset_load_timeout_seconds()) {
  timeout_sec <- suppressWarnings(as.numeric(timeout_sec)[[1]])
  if (!is.finite(timeout_sec) || timeout_sec <= 0) {
    return(force(expr))
  }
  setTimeLimit(elapsed = timeout_sec, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
  force(expr)
}
