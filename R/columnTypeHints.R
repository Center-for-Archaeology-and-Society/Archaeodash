#' Guess numeric-like columns quickly from a sample
#'
#' @param df data frame
#' @param exclude column names to ignore
#' @param sample_n number of rows to sample from the top
#' @param min_parse_rate minimum fraction of non-empty values that must parse as numeric
#'
#' @return character vector of column names that appear numeric
#' @export
guess_numeric_columns_fast <- function(df,
                                       exclude = c("rowid"),
                                       sample_n = 1000L,
                                       min_parse_rate = 0.95) {
  if (!inherits(df, "data.frame") || nrow(df) == 0) return(character())

  sample_n <- suppressWarnings(as.integer(sample_n[[1]]))
  if (!is.finite(sample_n) || sample_n <= 0) sample_n <- min(1000L, nrow(df))
  sample_n <- min(sample_n, nrow(df))
  min_parse_rate <- suppressWarnings(as.numeric(min_parse_rate[[1]]))
  if (!is.finite(min_parse_rate) || min_parse_rate <= 0 || min_parse_rate > 1) min_parse_rate <- 0.95

  idx <- seq_len(sample_n)
  cols <- setdiff(names(df), as.character(exclude))
  cols <- cols[!is.na(cols) & nzchar(cols)]
  if (length(cols) == 0) return(character())

  out <- character()
  for (nm in cols) {
    x <- df[[nm]]
    if (is.numeric(x) || is.integer(x)) {
      out <- c(out, nm)
      next
    }
    vals <- as.character(x[idx])
    vals <- trimws(vals)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) next
    parsed <- suppressWarnings(as.numeric(vals))
    parse_rate <- mean(!is.na(parsed))
    if (isTRUE(parse_rate >= min_parse_rate)) {
      out <- c(out, nm)
    }
  }
  unique(out)
}
