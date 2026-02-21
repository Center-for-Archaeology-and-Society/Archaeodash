rename_table_safe <- function(con, from_name, to_name) {
  if (is.null(con) || !nzchar(from_name) || !nzchar(to_name) || identical(from_name, to_name)) return(FALSE)
  tbls <- tryCatch(DBI::dbListTables(con), error = function(e) character())
  if (!(from_name %in% tbls) || (to_name %in% tbls)) return(FALSE)
  from_sql <- as.character(DBI::dbQuoteIdentifier(con, from_name))
  to_sql <- as.character(DBI::dbQuoteIdentifier(con, to_name))
  tryCatch({
    DBI::dbExecute(con, paste0("RENAME TABLE ", from_sql, " TO ", to_sql))
    TRUE
  }, error = function(e) FALSE)
}

shorten_existing_table_name <- function(name, max_len = app_table_name_max_len) {
  nm <- janitor::make_clean_names(as.character(name))
  if (nchar(nm) <= max_len) return(nm)
  hash <- dataset_table_name_hash(nm, n = 8)
  paste0(substr(nm, 1, max(1, max_len - 1 - nchar(hash))), "_", hash)
}

migrate_table_names_to_32 <- function(con) {
  if (is.null(con)) return(invisible(list(renamed = character(), skipped = character())))

  renamed <- character()
  skipped <- character()

  record_rename <- function(from, to) {
    renamed <<- c(renamed, paste0(from, " -> ", to))
  }
  record_skip <- function(msg) {
    skipped <<- c(skipped, msg)
  }

  # 1) Keep preference and transformation index table names aligned with the new 32-char policy.
  tbls <- tryCatch(DBI::dbListTables(con), error = function(e) character())
  pref_tables <- tbls[grepl("_preferences$", tbls)]
  for (old_tbl in pref_tables) {
    username <- sub("_preferences$", "", old_tbl)
    new_tbl <- build_user_preferences_table_name(username, max_len = app_table_name_max_len)
    if (identical(old_tbl, new_tbl)) next
    if (rename_table_safe(con, old_tbl, new_tbl)) {
      record_rename(old_tbl, new_tbl)
    } else {
      record_skip(paste0("preferences table not renamed: ", old_tbl, " -> ", new_tbl))
    }
  }

  tbls <- tryCatch(DBI::dbListTables(con), error = function(e) character())
  idx_tables <- tbls[grepl("_transformations$", tbls)]
  for (old_tbl in idx_tables) {
    username <- sub("_transformations$", "", old_tbl)
    new_tbl <- transform_index_table(username)
    if (identical(old_tbl, new_tbl)) next
    if (rename_table_safe(con, old_tbl, new_tbl)) {
      record_rename(old_tbl, new_tbl)
    } else {
      record_skip(paste0("index table not renamed: ", old_tbl, " -> ", new_tbl))
    }
  }

  # 2) Migrate persisted transformation table prefixes to the new 32-char policy.
  tbls <- tryCatch(DBI::dbListTables(con), error = function(e) character())
  idx_tables <- tbls[grepl("_transformations$", tbls)]
  for (idx_tbl in idx_tables) {
    username <- sub("_transformations$", "", idx_tbl)
    rows <- tryCatch(
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT dataset_key, transformation_name, table_prefix FROM ",
          as.character(DBI::dbQuoteIdentifier(con, idx_tbl))
        )
      ),
      error = function(e) data.frame()
    )
    if (!inherits(rows, "data.frame") || nrow(rows) == 0) next

    for (i in seq_len(nrow(rows))) {
      old_prefix <- as.character(rows$table_prefix[[i]])
      dataset_key <- as.character(rows$dataset_key[[i]])
      tx_name <- as.character(rows$transformation_name[[i]])
      new_prefix <- transform_prefix(username, dataset_key, tx_name)
      if (!nzchar(old_prefix) || identical(old_prefix, new_prefix)) next

      for (sfx in transform_table_suffixes) {
        old_tbl <- paste0(old_prefix, sfx)
        new_tbl <- paste0(new_prefix, sfx)
        if (identical(old_tbl, new_tbl)) next
        if (rename_table_safe(con, old_tbl, new_tbl)) {
          record_rename(old_tbl, new_tbl)
        }
      }

      ok_update <- tryCatch({
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE ", as.character(DBI::dbQuoteIdentifier(con, idx_tbl)),
            " SET table_prefix = ", DBI::dbQuoteString(con, new_prefix),
            " WHERE dataset_key = ", DBI::dbQuoteString(con, dataset_key),
            " AND transformation_name = ", DBI::dbQuoteString(con, tx_name)
          )
        )
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(ok_update)) {
        record_rename(paste0(idx_tbl, ".table_prefix:", old_prefix), new_prefix)
      } else {
        record_skip(paste0("table_prefix update failed: ", idx_tbl, " ", old_prefix, " -> ", new_prefix))
      }
    }
  }

  # 3) Rename overlong dataset base tables and their metadata companions.
  tbls <- tryCatch(DBI::dbListTables(con), error = function(e) character())
  is_metadata <- grepl("_metadata$", tbls)
  is_pref <- grepl("_preferences$", tbls)
  is_idx <- grepl("_transformations$", tbls)
  is_tx_data <- grepl("_tx_", tbls) | grepl("_(selected|selected_all|pca|umap|lda|meta)$", tbls)
  is_system <- tbls %in% c("users", "remember_tokens")
  dataset_bases <- tbls[!(is_metadata | is_pref | is_idx | is_tx_data | is_system)]
  dataset_bases <- dataset_bases[grepl("_", dataset_bases)]

  for (old_base in dataset_bases) {
    if (nchar(old_base) <= app_table_name_max_len) next
    username <- sub("_.*$", "", old_base)
    label <- sub(paste0("^", username, "_"), "", old_base)
    new_base <- build_dataset_table_name(username = username, dataset_label = label, max_len = app_table_name_max_len)
    if (identical(old_base, new_base)) next

    if (rename_table_safe(con, old_base, new_base)) {
      record_rename(old_base, new_base)
    } else {
      record_skip(paste0("dataset table not renamed: ", old_base, " -> ", new_base))
      next
    }

    old_meta <- paste0(old_base, "_metadata")
    new_meta <- paste0(new_base, "_metadata")
    if (rename_table_safe(con, old_meta, new_meta)) {
      record_rename(old_meta, new_meta)
    }

    pref_tbl <- build_user_preferences_table_name(username, max_len = app_table_name_max_len)
    if (pref_tbl %in% tryCatch(DBI::dbListTables(con), error = function(e) character())) {
      try(
        DBI::dbExecute(
          con,
          paste0(
            "UPDATE ", as.character(DBI::dbQuoteIdentifier(con, pref_tbl)),
            " SET value = ", DBI::dbQuoteString(con, new_base),
            " WHERE field = 'lastOpenedDataset' AND value = ", DBI::dbQuoteString(con, old_base)
          )
        ),
        silent = TRUE
      )
    }
  }

  invisible(list(renamed = renamed, skipped = skipped))
}
