dataset_table_name_hash <- function(x, n = 8) {
  tf <- tempfile("archaeodash_dataset_name_")
  on.exit(unlink(tf), add = TRUE)
  writeBin(charToRaw(enc2utf8(as.character(x))), tf)
  substr(as.character(tools::md5sum(tf)[[1]]), 1, n)
}

build_dataset_table_name <- function(username, dataset_label, max_len = 54) {
  user <- janitor::make_clean_names(as.character(username))
  label <- janitor::make_clean_names(as.character(dataset_label))
  candidate <- paste0(user, "_", label)
  if (nchar(candidate) <= max_len) return(candidate)

  suffix <- dataset_table_name_hash(candidate, n = 8)
  base_budget <- max_len - nchar(user) - 1 - 1 - nchar(suffix)
  if (base_budget < 1) {
    user_budget <- max(1, max_len - 1 - 1 - nchar(suffix))
    user <- substr(user, 1, user_budget)
    base_budget <- 1
  }
  label_short <- substr(label, 1, base_budget)
  paste0(user, "_", label_short, "_", suffix)
}
