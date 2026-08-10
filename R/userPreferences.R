# Helpers for reading and writing user preference key/value records.
empty_user_preferences <- function() {
  tibble::tibble(field = character(), value = character())
}

read_user_preferences_safe <- function(con, username, max_len = app_table_name_max_len) {
  username <- as.character(username[[1]])
  if (is.null(con) || !nzchar(username)) return(empty_user_preferences())
  pref_tbl <- build_user_preferences_table_name(username, max_len = max_len)
  if (!db_table_exists_safe(con, pref_tbl)) return(empty_user_preferences())

  prefs <- tryCatch(
    dplyr::tbl(con, pref_tbl) %>% dplyr::collect() %>% dplyr::mutate_all(as.character),
    error = function(e) {
      app_log(paste0("read_user_preferences_safe: error table=", pref_tbl, " msg=", conditionMessage(e)))
      empty_user_preferences()
    }
  )
  if (!inherits(prefs, "data.frame")) return(empty_user_preferences())
  if (!all(c("field", "value") %in% names(prefs))) return(empty_user_preferences())

  prefs %>%
    dplyr::transmute(
      field = as.character(.data$field),
      value = as.character(.data$value)
    )
}

get_user_preference_safe <- function(con, username, field, default_value = "") {
  pref_field <- as.character(field[[1]])
  if (!nzchar(pref_field)) return(as.character(default_value[[1]]))
  prefs <- read_user_preferences_safe(con = con, username = username)
  if (nrow(prefs) == 0) return(as.character(default_value[[1]]))
  val <- prefs %>%
    dplyr::filter(.data$field == pref_field) %>%
    dplyr::pull(.data$value)
  if (length(val) == 0 || is.na(val[[1]])) return(as.character(default_value[[1]]))
  as.character(val[[1]])
}

write_user_preference_safe <- function(con, username, field, value, max_len = app_table_name_max_len) {
  username <- as.character(username[[1]])
  pref_field <- as.character(field[[1]])
  pref_value <- as.character(value[[1]])
  if (is.null(con) || !nzchar(username) || !nzchar(pref_field)) return(FALSE)
  if (!app_require_packages("DBI", feature = "Saving user preferences")) return(FALSE)

  pref_tbl <- build_user_preferences_table_name(username, max_len = max_len)
  if (!db_table_exists_safe(con, pref_tbl)) {
    return(isTRUE(db_write_table_safe(
      con = con,
      table_name = pref_tbl,
      value = tibble::tibble(field = pref_field, value = pref_value),
      row.names = FALSE,
      context = "creating user preferences table"
    )))
  }

  tbl_sql <- DBI::SQL(as.character(DBI::dbQuoteIdentifier(con, pref_tbl)))
  update_query <- DBI::sqlInterpolate(
    con,
    "UPDATE ?tbl SET value = ?value WHERE field = ?field",
    tbl = tbl_sql,
    value = pref_value,
    field = pref_field
  )
  updated <- tryCatch(
    DBI::dbExecute(con, update_query),
    error = function(e) {
      app_log(paste0("write_user_preference_safe: update error table=", pref_tbl, " field=", pref_field, " msg=", conditionMessage(e)))
      NA_integer_
    }
  )
  if (!is.na(updated) && updated > 0) return(TRUE)

  insert_query <- DBI::sqlInterpolate(
    con,
    "INSERT INTO ?tbl (field, value) VALUES (?field, ?value)",
    tbl = tbl_sql,
    field = pref_field,
    value = pref_value
  )
  inserted <- tryCatch(
    DBI::dbExecute(con, insert_query),
    error = function(e) {
      app_log(paste0("write_user_preference_safe: insert error table=", pref_tbl, " field=", pref_field, " msg=", conditionMessage(e)))
      NA_integer_
    }
  )
  if (!is.na(inserted)) return(TRUE)

  # Last-resort schema-safe fallback for legacy/non-conforming preference tables.
  prefs <- read_user_preferences_safe(con = con, username = username, max_len = max_len) %>%
    dplyr::filter(.data$field != pref_field) %>%
    dplyr::bind_rows(tibble::tibble(field = pref_field, value = pref_value))
  isTRUE(db_write_table_safe(
    con = con,
    table_name = pref_tbl,
    value = prefs,
    row.names = FALSE,
    overwrite = TRUE,
    context = "saving user preferences fallback"
  ))
}
