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
#'
#' @return data frame with refreshed metadata columns
refreshNonElementMetadata <- function(transformed, imported_data, chem) {
  if (!inherits(transformed, "data.frame") || !inherits(imported_data, "data.frame")) {
    return(transformed)
  }
  if (!("rowid" %in% names(transformed)) || !("rowid" %in% names(imported_data))) {
    return(transformed)
  }

  metadata_cols <- setdiff(names(imported_data), chem)
  metadata_cols <- unique(c("rowid", metadata_cols))
  metadata_tbl <- imported_data %>%
    dplyr::select(tidyselect::any_of(metadata_cols))

  replace_cols <- setdiff(colnames(metadata_tbl), "rowid")
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
    ratioSpecs = if (inherits(rvals$ratioSpecs, "data.frame")) rvals$ratioSpecs else tibble::tibble(),
    ratioMode = if (is.null(rvals$ratioMode)) "append" else as.character(rvals$ratioMode),
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
  rvals$ratioSpecs <- if (inherits(snapshot$ratioSpecs, "data.frame")) snapshot$ratioSpecs else tibble::tibble()
  rvals$ratioMode <- if (is.null(snapshot$ratioMode) || !snapshot$ratioMode %in% c("append", "only")) "append" else snapshot$ratioMode
  rvals$selectedData <- snapshot$selectedData
  rvals$pcadf <- snapshot$pcadf
  rvals$umapdf <- snapshot$umapdf
  rvals$LDAdf <- snapshot$LDAdf
  rvals$pca <- snapshot$pca
  rvals$LDAmod <- snapshot$LDAmod
  rvals$activeTransformation <- snapshot$name
  invisible(NULL)
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
    if (is.null(snapshot$selectedData)) return(snapshot)
    snapshot$selectedData <- refreshNonElementMetadata(snapshot$selectedData, imported_data, chem)
    snapshot$pcadf <- refreshNonElementMetadata(snapshot$pcadf, imported_data, chem)
    snapshot$umapdf <- refreshNonElementMetadata(snapshot$umapdf, imported_data, chem)
    snapshot$LDAdf <- refreshNonElementMetadata(snapshot$LDAdf, imported_data, chem)
    snapshot
  })
}
