archaeo_test_app_dir <- function() {
  source_dir <- tryCatch(
    normalizePath(testthat::test_path("..", "..", "inst", "app"), mustWork = FALSE),
    error = function(e) ""
  )
  if (is.character(source_dir) && nzchar(source_dir) && dir.exists(source_dir)) {
    return(source_dir)
  }

  installed_dir <- system.file("app", package = "ArchaeoDash")
  if (is.character(installed_dir) && nzchar(installed_dir) && dir.exists(installed_dir)) {
    return(normalizePath(installed_dir, mustWork = TRUE))
  }

  testthat::skip("Unable to locate app directory in source tree or installed package.")
}

archaeo_test_app_file <- function(...) {
  normalizePath(file.path(archaeo_test_app_dir(), ...), mustWork = TRUE)
}
