# Lightweight runtime timing logs for operational debugging.
# Enabled by default; disable with ARCHAEODASH_TIMING_LOG=0.

app_timing_enabled <- function() {
  flag <- tolower(trimws(Sys.getenv("ARCHAEODASH_TIMING_LOG", "1")))
  !(flag %in% c("0", "false", "off", "no"))
}

timing_elapsed_ms <- function(start_time) {
  as.integer(round(as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000))
}

app_timing_log <- function(event, fields = list()) {
  if (!isTRUE(app_timing_enabled())) return(invisible(NULL))

  if (!is.list(fields)) fields <- list(value = fields)
  field_text <- character()
  if (length(fields) > 0) {
    nm <- names(fields)
    if (is.null(nm)) nm <- rep("", length(fields))
    field_text <- vapply(seq_along(fields), function(i) {
      key <- if (nzchar(nm[[i]])) nm[[i]] else paste0("f", i)
      value <- fields[[i]]
      value_chr <- if (length(value) == 0 || is.null(value) || is.na(value[[1]])) "NA" else as.character(value[[1]])
      paste0(key, "=", value_chr)
    }, character(1))
  }

  msg <- paste0(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    " [timing] ",
    as.character(event),
    if (length(field_text) > 0) paste0(" ", paste(field_text, collapse = " ")) else ""
  )
  base::message(msg)
  invisible(NULL)
}
