#' Ensure a data frame has a usable rowid column
#'
#' @param data data frame-like object
#' @param table_name label used in messages
#' @param reference_rowid optional reference rowid vector to reuse
#' @param require_unique logical; whether rowid should be unique
#' @param allow_long logical; if TRUE, duplicate rowid values are allowed
#'
#' @return data frame with rowid column
#' @export
ensure_rowid_column <- function(data,
                                table_name = "data",
                                reference_rowid = NULL,
                                require_unique = TRUE,
                                allow_long = FALSE) {
  if (!inherits(data, "data.frame")) return(data)

  has_valid_reference <- !is.null(reference_rowid) && length(reference_rowid) == nrow(data)

  if (!("rowid" %in% names(data))) {
    if (has_valid_reference) {
      data$rowid <- as.character(reference_rowid)
      message(glue::glue("added missing rowid to {table_name} from reference rowid"))
    } else {
      data <- tibble::rowid_to_column(data, var = "rowid")
      message(glue::glue("added missing rowid to {table_name} using row index"))
    }
  } else {
    data$rowid <- as.character(data$rowid)
  }

  # Replace missing/blank ids with deterministic sequential ids.
  invalid_idx <- which(is.na(data$rowid) | !nzchar(data$rowid))
  if (length(invalid_idx) > 0) {
    data$rowid[invalid_idx] <- as.character(invalid_idx)
    message(glue::glue("repaired {length(invalid_idx)} invalid rowid values in {table_name}"))
  }

  if (isTRUE(require_unique) && !isTRUE(allow_long)) {
    if (anyDuplicated(data$rowid) > 0) {
      data$rowid <- as.character(seq_len(nrow(data)))
      message(glue::glue("rowid was not unique in {table_name}; reassigned sequential unique rowid"))
    }
  }

  data
}

#' Enforce rowid integrity across reactive data tables
#'
#' @param rvals reactive values object
#'
#' @return invisible NULL
#' @export
ensure_core_rowids <- function(rvals) {
  if (is.null(rvals)) return(invisible(NULL))

  if (inherits(rvals$importedData, "data.frame")) {
    rvals$importedData <- ensure_rowid_column(
      data = rvals$importedData,
      table_name = "importedData",
      require_unique = TRUE,
      allow_long = FALSE
    )
  }

  selected_reference <- NULL
  if (inherits(rvals$importedData, "data.frame") &&
      inherits(rvals$selectedData, "data.frame") &&
      nrow(rvals$importedData) == nrow(rvals$selectedData)) {
    selected_reference <- rvals$importedData$rowid
  }

  if (inherits(rvals$selectedData, "data.frame")) {
    rvals$selectedData <- ensure_rowid_column(
      data = rvals$selectedData,
      table_name = "selectedData",
      reference_rowid = selected_reference,
      require_unique = TRUE,
      allow_long = FALSE
    )
  }

  derived_tables <- c("pcadf", "umapdf", "LDAdf")
  for (nm in derived_tables) {
    if (!inherits(rvals[[nm]], "data.frame")) next
    ref <- NULL
    if (inherits(rvals$selectedData, "data.frame") && nrow(rvals[[nm]]) == nrow(rvals$selectedData)) {
      ref <- rvals$selectedData$rowid
    }
    rvals[[nm]] <- ensure_rowid_column(
      data = rvals[[nm]],
      table_name = nm,
      reference_rowid = ref,
      require_unique = TRUE,
      allow_long = FALSE
    )
  }

  long_tables <- c("membershipProbs", "edistance")
  for (nm in long_tables) {
    if (!inherits(rvals[[nm]], "data.frame")) next
    rvals[[nm]] <- ensure_rowid_column(
      data = rvals[[nm]],
      table_name = nm,
      require_unique = FALSE,
      allow_long = TRUE
    )
  }

  invisible(NULL)
}
