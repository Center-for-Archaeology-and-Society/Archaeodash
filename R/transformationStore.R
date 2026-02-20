#' Build a default transformation name
#'
#' @return character scalar
defaultTransformationName <- function() {
  paste0("transformation_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

#' Refresh non-element metadata in a transformed table
#'
#' @param transformed data frame that may include transformed element columns
#' @param imported_data canonical uploaded data with latest metadata
#' @param chem character vector of element columns
#' @param preserve_cols metadata columns to preserve from `transformed`
#'
#' @return data frame with refreshed metadata columns
refreshNonElementMetadata <- function(transformed, imported_data, chem, preserve_cols = character()) {
  if (!inherits(transformed, "data.frame") || !inherits(imported_data, "data.frame")) {
    return(transformed)
  }
  if (!("rowid" %in% names(transformed)) || !("rowid" %in% names(imported_data))) {
    return(transformed)
  }

  metadata_cols <- setdiff(names(imported_data), chem)
  metadata_cols <- unique(c("rowid", metadata_cols))
  preserve_cols <- unique(as.character(preserve_cols))
  preserve_cols <- preserve_cols[!is.na(preserve_cols) & nzchar(preserve_cols)]
  metadata_tbl <- imported_data %>%
    dplyr::select(tidyselect::any_of(setdiff(metadata_cols, preserve_cols)))

  replace_cols <- setdiff(colnames(metadata_tbl), c("rowid", preserve_cols))
  transformed %>%
    dplyr::mutate(rowid = as.character(.data$rowid)) %>%
    dplyr::select(-tidyselect::any_of(replace_cols)) %>%
    dplyr::left_join(
      metadata_tbl %>% dplyr::mutate(rowid = as.character(.data$rowid)),
      by = "rowid"
    )
}

#' Build an in-session transformation snapshot
#'
#' @param rvals reactive values object
#' @param name transformation name
#'
#' @return list snapshot
buildTransformationSnapshot <- function(rvals, name) {
  list(
    name = name,
    created = as.character(Sys.time()),
    chem = rvals$chem,
    attr = rvals$attr,
    attrs = rvals$attrs,
    attrGroups = rvals$attrGroups,
    attrGroupsSub = rvals$attrGroupsSub,
    transform.method = rvals$transform.method,
    impute.method = rvals$impute.method,
    runPCA = isTRUE(rvals$runPCA),
    runUMAP = isTRUE(rvals$runUMAP),
    runLDA = isTRUE(rvals$runLDA),
    data.src = if (is.null(rvals$data.src)) "" else as.character(rvals$data.src),
    xvar = if (is.null(rvals$xvar)) "" else as.character(rvals$xvar),
    yvar = if (is.null(rvals$yvar)) "" else as.character(rvals$yvar),
    xvar2 = if (is.null(rvals$xvar2)) character() else as.character(rvals$xvar2),
    yvar2 = if (is.null(rvals$yvar2)) character() else as.character(rvals$yvar2),
    Conf = isTRUE(rvals$Conf),
    int.set = if (is.null(rvals$int.set)) 0.95 else as.numeric(rvals$int.set[[1]]),
    plot_theme = if (is.null(rvals$plot_theme)) "viridis" else as.character(rvals$plot_theme),
    use_symbols = if (is.null(rvals$use_symbols)) TRUE else isTRUE(rvals$use_symbols),
    show_point_labels = if (is.null(rvals$show_point_labels)) FALSE else isTRUE(rvals$show_point_labels),
    pointLabelColumn = if (is.null(rvals$pointLabelColumn)) "" else as.character(rvals$pointLabelColumn),
    ratioSpecs = if (inherits(rvals$ratioSpecs, "data.frame")) rvals$ratioSpecs else tibble::tibble(),
    ratioMode = if (is.null(rvals$ratioMode)) "append" else as.character(rvals$ratioMode),
    selectedDataAll = if (inherits(rvals$selectedDataAll, "data.frame")) rvals$selectedDataAll else rvals$selectedData,
    selectedData = rvals$selectedData,
    pcadf = rvals$pcadf,
    umapdf = rvals$umapdf,
    LDAdf = rvals$LDAdf,
    pca = rvals$pca,
    LDAmod = rvals$LDAmod
  )
}

#' Apply a transformation snapshot to reactive values
#'
#' @param rvals reactive values object
#' @param snapshot transformation snapshot
#'
#' @return NULL
applyTransformationSnapshot <- function(rvals, snapshot) {
  if (is.null(snapshot) || !is.list(snapshot)) return(invisible(NULL))
  rvals$chem <- snapshot$chem
  rvals$attr <- snapshot$attr
  rvals$attrs <- snapshot$attrs
  rvals$attrGroups <- snapshot$attrGroups
  rvals$attrGroupsSub <- snapshot$attrGroupsSub
  rvals$transform.method <- snapshot$transform.method
  rvals$impute.method <- snapshot$impute.method
  rvals$runPCA <- snapshot$runPCA
  rvals$runUMAP <- snapshot$runUMAP
  rvals$runLDA <- snapshot$runLDA
  rvals$data.src <- if (is.null(snapshot$data.src) || !nzchar(as.character(snapshot$data.src))) NULL else as.character(snapshot$data.src)
  rvals$xvar <- if (is.null(snapshot$xvar) || !nzchar(as.character(snapshot$xvar))) NULL else as.character(snapshot$xvar)
  rvals$yvar <- if (is.null(snapshot$yvar) || !nzchar(as.character(snapshot$yvar))) NULL else as.character(snapshot$yvar)
  rvals$xvar2 <- if (is.null(snapshot$xvar2) || length(snapshot$xvar2) == 0) NULL else as.character(snapshot$xvar2)
  rvals$yvar2 <- if (is.null(snapshot$yvar2) || length(snapshot$yvar2) == 0) NULL else as.character(snapshot$yvar2)
  rvals$Conf <- isTRUE(snapshot$Conf)
  rvals$int.set <- if (is.null(snapshot$int.set) || !is.finite(as.numeric(snapshot$int.set[[1]]))) NULL else as.numeric(snapshot$int.set[[1]])
  rvals$plot_theme <- if (is.null(snapshot$plot_theme) || !nzchar(as.character(snapshot$plot_theme))) NULL else as.character(snapshot$plot_theme)
  rvals$use_symbols <- if (is.null(snapshot$use_symbols)) TRUE else isTRUE(snapshot$use_symbols)
  rvals$show_point_labels <- if (is.null(snapshot$show_point_labels)) FALSE else isTRUE(snapshot$show_point_labels)
  rvals$pointLabelColumn <- if (is.null(snapshot$pointLabelColumn) || !nzchar(as.character(snapshot$pointLabelColumn))) NULL else as.character(snapshot$pointLabelColumn)
  rvals$ratioSpecs <- if (inherits(snapshot$ratioSpecs, "data.frame")) snapshot$ratioSpecs else tibble::tibble()
  rvals$ratioMode <- if (is.null(snapshot$ratioMode) || !snapshot$ratioMode %in% c("append", "only")) "append" else snapshot$ratioMode
  rvals$selectedDataAll <- if (inherits(snapshot$selectedDataAll, "data.frame")) snapshot$selectedDataAll else snapshot$selectedData
  rvals$selectedData <- snapshot$selectedData
  rvals$pcadf <- snapshot$pcadf
  rvals$umapdf <- snapshot$umapdf
  rvals$LDAdf <- snapshot$LDAdf
  rvals$pca <- snapshot$pca
  rvals$LDAmod <- snapshot$LDAmod
  rvals$activeTransformation <- snapshot$name
  invisible(NULL)
}

applyTransformationGroupFilter <- function(snapshot) {
  if (is.null(snapshot) || !is.list(snapshot)) return(snapshot)
  base_df <- if (inherits(snapshot$selectedDataAll, "data.frame")) snapshot$selectedDataAll else snapshot$selectedData
  if (!inherits(base_df, "data.frame")) return(snapshot)

  if (!("rowid" %in% names(base_df))) {
    snapshot$selectedData <- base_df
    return(snapshot)
  }

  base_df$rowid <- as.character(base_df$rowid)
  group_col <- as.character(snapshot$attrGroups)
  selected_groups <- as.character(snapshot$attrGroupsSub)
  selected_groups <- selected_groups[!is.na(selected_groups) & nzchar(selected_groups)]

  if (nzchar(group_col) && group_col %in% names(base_df) && length(selected_groups) > 0) {
    keep_rowids <- base_df %>%
      dplyr::mutate(.group_value = as.character(.data[[group_col]])) %>%
      dplyr::filter(.data$.group_value %in% selected_groups) %>%
      dplyr::pull(.data$rowid) %>%
      as.character() %>%
      unique()
  } else {
    keep_rowids <- as.character(base_df$rowid)
  }

  snapshot$selectedData <- base_df %>%
    dplyr::filter(.data$rowid %in% keep_rowids)

  filter_by_rowid <- function(df) {
    if (!inherits(df, "data.frame") || !("rowid" %in% names(df))) return(df)
    df %>%
      dplyr::mutate(rowid = as.character(.data$rowid)) %>%
      dplyr::filter(.data$rowid %in% keep_rowids)
  }
  snapshot$pcadf <- filter_by_rowid(snapshot$pcadf)
  snapshot$umapdf <- filter_by_rowid(snapshot$umapdf)
  snapshot$LDAdf <- filter_by_rowid(snapshot$LDAdf)
  snapshot
}

#' Refresh metadata in all stored transformations
#'
#' @param transformations named list of snapshots
#' @param imported_data canonical uploaded data
#' @param chem current element columns
#'
#' @return named list
refreshTransformationMetadata <- function(transformations, imported_data, chem) {
  if (is.null(transformations) || length(transformations) == 0) return(list())
  lapply(transformations, function(snapshot) {
    if (is.null(snapshot$selectedData) && is.null(snapshot$selectedDataAll)) return(snapshot)
    snapshot$selectedDataAll <- refreshNonElementMetadata(
      transformed = if (inherits(snapshot$selectedDataAll, "data.frame")) snapshot$selectedDataAll else snapshot$selectedData,
      imported_data = imported_data,
      chem = chem,
      preserve_cols = character()
    )
    snapshot$pcadf <- refreshNonElementMetadata(snapshot$pcadf, imported_data, chem, preserve_cols = character())
    snapshot$umapdf <- refreshNonElementMetadata(snapshot$umapdf, imported_data, chem, preserve_cols = character())
    snapshot$LDAdf <- refreshNonElementMetadata(snapshot$LDAdf, imported_data, chem, preserve_cols = character())
    snapshot <- applyTransformationGroupFilter(snapshot)
    snapshot
  })
}
