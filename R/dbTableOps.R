# Safe DB table operations used across modules to reduce duplicated write/remove logic.
db_table_exists_safe <- function(con, table_name) {
  if (is.null(con) || is.null(table_name) || !nzchar(as.character(table_name))) {
    return(FALSE)
  }
  tryCatch(DBI::dbExistsTable(con, as.character(table_name)), error = function(e) FALSE)
}

db_write_table_safe <- function(con,
                                table_name,
                                value,
                                row.names = FALSE,
                                overwrite = FALSE,
                                append = FALSE,
                                context = "database write") {
  if (!app_require_packages("DBI", feature = context)) return(FALSE)
  if (is.null(con) || is.null(table_name) || !nzchar(as.character(table_name))) return(FALSE)
  ok <- tryCatch({
    DBI::dbWriteTable(
      conn = con,
      name = as.character(table_name),
      value = value,
      row.names = row.names,
      overwrite = overwrite,
      append = append
    )
    TRUE
  }, error = function(e) {
    mynotification(paste0("Failed ", context, " for table '", table_name, "': ", e$message), type = "error")
    FALSE
  })
  isTRUE(ok)
}

db_remove_table_safe <- function(con, table_name, context = "database delete") {
  if (!app_require_packages("DBI", feature = context)) return(FALSE)
  if (is.null(con) || is.null(table_name) || !nzchar(as.character(table_name))) return(FALSE)
  if (!db_table_exists_safe(con, table_name)) return(TRUE)
  ok <- tryCatch({
    DBI::dbRemoveTable(con, as.character(table_name))
    TRUE
  }, error = function(e) {
    mynotification(paste0("Failed ", context, " for table '", table_name, "': ", e$message), type = "error")
    FALSE
  })
  isTRUE(ok)
}
