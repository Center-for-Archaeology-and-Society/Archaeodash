transformation_sep <- "\u001f"

collapse_transform_values <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  paste(x, collapse = transformation_sep)
}

split_transform_values <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character())
  strsplit(x, transformation_sep, fixed = TRUE)[[1]]
}

encode_ratio_specs <- function(specs) {
  if (!inherits(specs, "data.frame") || nrow(specs) == 0) return("")
  required <- c("ratio", "numerator", "denominator")
  if (!all(required %in% names(specs))) return("")
  rows <- apply(specs[, required, drop = FALSE], 1, function(r) {
    paste(as.character(r), collapse = "|")
  })
  paste(rows, collapse = transformation_sep)
}

decode_ratio_specs <- function(x) {
  if (is.null(x) || !nzchar(x)) return(tibble::tibble())
  rows <- strsplit(x, transformation_sep, fixed = TRUE)[[1]]
  parsed <- lapply(rows, function(row) strsplit(row, "|", fixed = TRUE)[[1]])
  parsed <- parsed[vapply(parsed, length, integer(1)) == 3]
  if (length(parsed) == 0) return(tibble::tibble())
  tibble::tibble(
    ratio = vapply(parsed, `[[`, character(1), 1),
    numerator = vapply(parsed, `[[`, character(1), 2),
    denominator = vapply(parsed, `[[`, character(1), 3)
  )
}

safe_table_name <- function(x, max_len = 63) {
  cleaned <- janitor::make_clean_names(as.character(x))
  substr(cleaned, 1, max_len)
}

build_dataset_key <- function(dataset_names) {
  if (is.null(dataset_names) || length(dataset_names) == 0) return("")
  normalized <- sort(unique(as.character(dataset_names)))
  canonical <- paste(normalized, collapse = "__")
  tf <- tempfile("archaeodash_dataset_key_")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(canonical)), tf)
  hash <- as.character(tools::md5sum(tf)[[1]])
  label <- if (length(normalized) == 1) {
    safe_table_name(normalized[[1]], max_len = 18)
  } else {
    paste0("multi", length(normalized))
  }
  safe_table_name(paste0("dsk_", label, "_", substr(hash, 1, 16)), max_len = 40)
}

transform_index_table <- function(username) {
  safe_table_name(paste0(username, "_transformations"), max_len = 63)
}

transform_prefix <- function(username, dataset_key, transformation_name) {
  safe_table_name(paste(username, "tx", dataset_key, transformation_name, sep = "_"), max_len = 55)
}

ensure_transform_index_table <- function(con, username) {
  idx <- transform_index_table(username)
  if (!DBI::dbExistsTable(con, idx)) {
    db_write_table_safe(
      con = con,
      table_name = idx,
      value = tibble::tibble(
        dataset_key = character(),
        transformation_name = character(),
        table_prefix = character(),
        created = character()
      ),
      row.names = FALSE,
      context = "creating transformation index table"
    )
  }
}

persist_transformation_db <- function(con, username, dataset_key, snapshot) {
  if (is.null(con) || !nzchar(username) || !nzchar(dataset_key) || is.null(snapshot$name)) {
    return(invisible(FALSE))
  }
  if (!app_require_packages("DBI", feature = "Persistent transformations")) {
    return(invisible(FALSE))
  }

  ensure_transform_index_table(con, username)
  opt_chr <- function(x, default = "") {
    if (is.null(x) || length(x) == 0 || is.na(x[[1]])) default else as.character(x[[1]])
  }

  tx_name <- as.character(snapshot$name[[1]])
  prefix <- transform_prefix(username, dataset_key, tx_name)
  selected_tbl <- paste0(prefix, "_selected")
  pca_tbl <- paste0(prefix, "_pca")
  umap_tbl <- paste0(prefix, "_umap")
  lda_tbl <- paste0(prefix, "_lda")
  meta_tbl <- paste0(prefix, "_meta")
  idx_tbl <- transform_index_table(username)
  idx_tbl_sql <- as.character(DBI::dbQuoteIdentifier(con, idx_tbl))

  if (!db_write_table_safe(
    con = con,
    table_name = selected_tbl,
    value = snapshot$selectedData,
    overwrite = TRUE,
    row.names = FALSE,
    context = "persisting selected transformation dataset"
  )) return(invisible(FALSE))

  if (inherits(snapshot$pcadf, "data.frame") && nrow(snapshot$pcadf) > 0) {
    if (!db_write_table_safe(
      con = con,
      table_name = pca_tbl,
      value = snapshot$pcadf,
      overwrite = TRUE,
      row.names = FALSE,
      context = "persisting PCA transformation dataset"
    )) return(invisible(FALSE))
  } else if (DBI::dbExistsTable(con, pca_tbl)) {
    if (!db_remove_table_safe(con, pca_tbl, context = "removing prior PCA transformation dataset")) return(invisible(FALSE))
  }

  if (inherits(snapshot$umapdf, "data.frame") && nrow(snapshot$umapdf) > 0) {
    if (!db_write_table_safe(
      con = con,
      table_name = umap_tbl,
      value = snapshot$umapdf,
      overwrite = TRUE,
      row.names = FALSE,
      context = "persisting UMAP transformation dataset"
    )) return(invisible(FALSE))
  } else if (DBI::dbExistsTable(con, umap_tbl)) {
    if (!db_remove_table_safe(con, umap_tbl, context = "removing prior UMAP transformation dataset")) return(invisible(FALSE))
  }

  if (inherits(snapshot$LDAdf, "data.frame") && nrow(snapshot$LDAdf) > 0) {
    if (!db_write_table_safe(
      con = con,
      table_name = lda_tbl,
      value = snapshot$LDAdf,
      overwrite = TRUE,
      row.names = FALSE,
      context = "persisting LDA transformation dataset"
    )) return(invisible(FALSE))
  } else if (DBI::dbExistsTable(con, lda_tbl)) {
    if (!db_remove_table_safe(con, lda_tbl, context = "removing prior LDA transformation dataset")) return(invisible(FALSE))
  }

  meta <- tibble::tibble(
    field = c(
      "dataset_key", "transformation_name", "created", "attrGroups",
      "transform.method", "impute.method", "runPCA", "runUMAP", "runLDA",
      "chem", "attr", "attrs", "attrGroupsSub", "ratioMode", "ratioSpecs",
      "data.src", "xvar", "yvar", "xvar2", "yvar2", "Conf", "int.set", "plot_theme",
      "use_symbols", "show_point_labels", "pointLabelColumn"
    ),
    value = c(
      dataset_key,
      tx_name,
      opt_chr(snapshot$created, as.character(Sys.time())),
      opt_chr(snapshot$attrGroups, ""),
      opt_chr(snapshot$transform.method, ""),
      opt_chr(snapshot$impute.method, ""),
      as.character(isTRUE(snapshot$runPCA)),
      as.character(isTRUE(snapshot$runUMAP)),
      as.character(isTRUE(snapshot$runLDA)),
      collapse_transform_values(snapshot$chem),
      collapse_transform_values(snapshot$attr),
      collapse_transform_values(snapshot$attrs),
      collapse_transform_values(snapshot$attrGroupsSub),
      opt_chr(snapshot$ratioMode, "append"),
      encode_ratio_specs(snapshot$ratioSpecs),
      opt_chr(snapshot$data.src, ""),
      opt_chr(snapshot$xvar, ""),
      opt_chr(snapshot$yvar, ""),
      collapse_transform_values(snapshot$xvar2),
      collapse_transform_values(snapshot$yvar2),
      as.character(isTRUE(snapshot$Conf)),
      opt_chr(snapshot$int.set, "0.95"),
      opt_chr(snapshot$plot_theme, "viridis"),
      as.character(isTRUE(snapshot$use_symbols)),
      as.character(isTRUE(snapshot$show_point_labels)),
      opt_chr(snapshot$pointLabelColumn, "")
    )
  )
  if (!db_write_table_safe(
    con = con,
    table_name = meta_tbl,
    value = meta,
    overwrite = TRUE,
    row.names = FALSE,
    context = "persisting transformation metadata"
  )) return(invisible(FALSE))

  quoted_name <- DBI::dbQuoteString(con, tx_name)
  quoted_key <- DBI::dbQuoteString(con, dataset_key)
  DBI::dbExecute(
    con,
    paste0(
      "DELETE FROM ", idx_tbl_sql,
      " WHERE dataset_key = ", quoted_key,
      " AND transformation_name = ", quoted_name
    )
  )
  if (!db_write_table_safe(
    con = con,
    table_name = idx_tbl,
    value = tibble::tibble(
      dataset_key = dataset_key,
      transformation_name = tx_name,
      table_prefix = prefix,
      created = opt_chr(snapshot$created, as.character(Sys.time()))
    ),
    append = TRUE,
    row.names = FALSE,
    context = "updating transformation index"
  )) return(invisible(FALSE))
  invisible(TRUE)
}

load_transformations_db <- function(con, username, dataset_key) {
  if (is.null(con) || !nzchar(username) || !nzchar(dataset_key)) return(list())
  if (!app_require_packages("DBI", feature = "Persistent transformations")) {
    return(list())
  }
  idx_tbl <- transform_index_table(username)
  idx_tbl_sql <- as.character(DBI::dbQuoteIdentifier(con, idx_tbl))
  if (!DBI::dbExistsTable(con, idx_tbl)) return(list())

  quoted_key <- DBI::dbQuoteString(con, dataset_key)
  rows <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", idx_tbl_sql, " WHERE dataset_key = ", quoted_key)
  )
  if (!is.data.frame(rows) || nrow(rows) == 0) return(list())

  snapshots <- list()
  for (i in seq_len(nrow(rows))) {
    tx_name <- as.character(rows$transformation_name[[i]])
    prefix <- as.character(rows$table_prefix[[i]])
    selected_tbl <- paste0(prefix, "_selected")
    meta_tbl <- paste0(prefix, "_meta")
    if (!DBI::dbExistsTable(con, selected_tbl)) next

    selected_data <- dplyr::tbl(con, selected_tbl) %>% dplyr::collect()
    pcadf <- if (DBI::dbExistsTable(con, paste0(prefix, "_pca"))) dplyr::tbl(con, paste0(prefix, "_pca")) %>% dplyr::collect() else tibble::tibble()
    umapdf <- if (DBI::dbExistsTable(con, paste0(prefix, "_umap"))) dplyr::tbl(con, paste0(prefix, "_umap")) %>% dplyr::collect() else tibble::tibble()
    LDAdf <- if (DBI::dbExistsTable(con, paste0(prefix, "_lda"))) dplyr::tbl(con, paste0(prefix, "_lda")) %>% dplyr::collect() else tibble::tibble()

    meta <- if (DBI::dbExistsTable(con, meta_tbl)) dplyr::tbl(con, meta_tbl) %>% dplyr::collect() else tibble::tibble(field = character(), value = character())
    get_meta <- function(field_name, default = "") {
      vals <- meta$value[meta$field == field_name]
      if (length(vals) == 0 || is.na(vals[[1]])) default else as.character(vals[[1]])
    }

    snapshots[[tx_name]] <- list(
      name = tx_name,
      created = get_meta("created", as.character(rows$created[[i]])),
      chem = split_transform_values(get_meta("chem", "")),
      attr = split_transform_values(get_meta("attr", "")),
      attrs = split_transform_values(get_meta("attrs", "")),
      attrGroups = get_meta("attrGroups", ""),
      attrGroupsSub = split_transform_values(get_meta("attrGroupsSub", "")),
      transform.method = get_meta("transform.method", ""),
      impute.method = get_meta("impute.method", ""),
      runPCA = identical(get_meta("runPCA", "FALSE"), "TRUE"),
      runUMAP = identical(get_meta("runUMAP", "FALSE"), "TRUE"),
      runLDA = identical(get_meta("runLDA", "FALSE"), "TRUE"),
      data.src = get_meta("data.src", "elements"),
      xvar = get_meta("xvar", ""),
      yvar = get_meta("yvar", ""),
      xvar2 = split_transform_values(get_meta("xvar2", "")),
      yvar2 = split_transform_values(get_meta("yvar2", "")),
      Conf = identical(get_meta("Conf", "FALSE"), "TRUE"),
      int.set = suppressWarnings(as.numeric(get_meta("int.set", "0.95"))),
      plot_theme = get_meta("plot_theme", "viridis"),
      use_symbols = identical(get_meta("use_symbols", "TRUE"), "TRUE"),
      show_point_labels = identical(get_meta("show_point_labels", "FALSE"), "TRUE"),
      pointLabelColumn = get_meta("pointLabelColumn", ""),
      ratioMode = get_meta("ratioMode", "append"),
      ratioSpecs = decode_ratio_specs(get_meta("ratioSpecs", "")),
      selectedData = selected_data,
      pcadf = pcadf,
      umapdf = umapdf,
      LDAdf = LDAdf,
      pca = NULL,
      LDAmod = NULL
    )
  }
  snapshots
}

delete_transformation_db <- function(con, username, dataset_key, transformation_name) {
  if (is.null(con) || !nzchar(username) || !nzchar(dataset_key) || !nzchar(transformation_name)) {
    return(invisible(FALSE))
  }
  if (!app_require_packages("DBI", feature = "Persistent transformations")) {
    return(invisible(FALSE))
  }
  idx_tbl <- transform_index_table(username)
  idx_tbl_sql <- as.character(DBI::dbQuoteIdentifier(con, idx_tbl))
  if (!DBI::dbExistsTable(con, idx_tbl)) return(invisible(FALSE))

  prefix <- transform_prefix(username, dataset_key, transformation_name)
  candidate_tables <- c(
    paste0(prefix, "_selected"),
    paste0(prefix, "_pca"),
    paste0(prefix, "_umap"),
    paste0(prefix, "_lda"),
    paste0(prefix, "_meta")
  )
  for (tbl in candidate_tables) {
    if (DBI::dbExistsTable(con, tbl)) {
      if (!db_remove_table_safe(con, tbl, context = "deleting persisted transformation table")) {
        return(invisible(FALSE))
      }
    }
  }

  quoted_name <- DBI::dbQuoteString(con, transformation_name)
  quoted_key <- DBI::dbQuoteString(con, dataset_key)
  DBI::dbExecute(
    con,
    paste0(
      "DELETE FROM ", idx_tbl_sql,
      " WHERE dataset_key = ", quoted_key,
      " AND transformation_name = ", quoted_name
    )
  )
  invisible(TRUE)
}

delete_transformations_for_dataset_db <- function(con, username, dataset_key) {
  if (is.null(con) || !nzchar(username) || !nzchar(dataset_key)) return(invisible(FALSE))
  if (!app_require_packages("DBI", feature = "Persistent transformations")) {
    return(invisible(FALSE))
  }
  idx_tbl <- transform_index_table(username)
  idx_tbl_sql <- as.character(DBI::dbQuoteIdentifier(con, idx_tbl))
  if (!DBI::dbExistsTable(con, idx_tbl)) return(invisible(FALSE))

  quoted_key <- DBI::dbQuoteString(con, dataset_key)
  rows <- DBI::dbGetQuery(con, paste0("SELECT transformation_name FROM ", idx_tbl_sql, " WHERE dataset_key = ", quoted_key))
  if (is.data.frame(rows) && nrow(rows) > 0) {
    for (tx_name in rows$transformation_name) {
      delete_transformation_db(con, username, dataset_key, as.character(tx_name))
    }
  }
  invisible(TRUE)
}
