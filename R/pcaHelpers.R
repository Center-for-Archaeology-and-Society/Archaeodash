#' Sort principal component column names numerically
#'
#' @param cols Character vector of column names.
#'
#' @return Ordered character vector of PC column names.
#' @keywords internal
pc_columns_sorted <- function(cols) {
  cols <- as.character(cols)
  cols <- cols[grepl("^PC[0-9]+$", cols)]
  if (length(cols) == 0) return(character())
  idx <- suppressWarnings(as.integer(sub("^PC", "", cols)))
  cols[order(idx, na.last = TRUE)]
}

#' Build PC count selectInput choices with variance labels
#'
#' @param pca_model PCA model object (e.g., prcomp result) or NULL.
#' @param pc_cols Character vector of PC column names.
#'
#' @return Named character vector where values are counts and labels include variance.
#' @keywords internal
membership_pc_count_choices <- function(pca_model, pc_cols) {
  pc_cols <- pc_columns_sorted(pc_cols)
  if (length(pc_cols) == 0) return(setNames(character(), character()))

  fallback <- setNames(
    as.character(seq_along(pc_cols)),
    paste0("First ", seq_along(pc_cols), " PCs")
  )

  if (is.null(pca_model) || is.null(pca_model$sdev)) return(fallback)
  var_vec <- as.numeric(pca_model$sdev)^2
  if (length(var_vec) == 0 || !is.finite(sum(var_vec)) || sum(var_vec) <= 0) return(fallback)

  n <- min(length(pc_cols), length(var_vec))
  var_pct <- (var_vec[seq_len(n)] / sum(var_vec)) * 100
  cum_pct <- cumsum(var_pct)
  labels <- vapply(seq_len(n), function(i) {
    sprintf(
      "First %d PCs (PC%d: %.1f%%; cumulative: %.1f%%)",
      i,
      i,
      var_pct[[i]],
      cum_pct[[i]]
    )
  }, character(1))
  setNames(as.character(seq_len(n)), labels)
}

#' Build PCA axis choices labeled with explained variance
#'
#' @param pca_model PCA model object (e.g., prcomp result) or NULL.
#' @param pc_cols Character vector of PC column names.
#'
#' @return Named character vector suitable for selectInput choices.
#' @keywords internal
pc_axis_choices_with_variance <- function(pca_model, pc_cols) {
  pc_cols <- pc_columns_sorted(pc_cols)
  if (length(pc_cols) == 0) return(setNames(character(), character()))
  labels <- pc_cols
  if (!is.null(pca_model) && !is.null(pca_model$sdev)) {
    var_vec <- as.numeric(pca_model$sdev)^2
    if (length(var_vec) > 0 && is.finite(sum(var_vec)) && sum(var_vec) > 0) {
      n <- min(length(pc_cols), length(var_vec))
      var_pct <- (var_vec[seq_len(n)] / sum(var_vec)) * 100
      labels <- paste0(pc_cols[seq_len(n)], " (", sprintf("%.1f%%", var_pct), ")")
      pc_cols <- pc_cols[seq_len(n)]
    }
  }
  setNames(pc_cols, labels)
}

#' Limit PC feature vector based on selected component count
#'
#' @param pc_cols Character vector of PC columns.
#' @param selected_count Selected number of PCs (numeric/integer/character).
#'
#' @return Character vector limited to selected count.
#' @keywords internal
limit_pc_features <- function(pc_cols, selected_count) {
  pc_cols <- pc_columns_sorted(pc_cols)
  if (length(pc_cols) == 0) return(character())
  n <- suppressWarnings(as.integer(selected_count)[[1]])
  if (is.na(n) || n < 1) n <- length(pc_cols)
  n <- min(n, length(pc_cols))
  pc_cols[seq_len(n)]
}
