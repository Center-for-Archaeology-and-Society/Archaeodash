ensure_shiny_dependency_aliases <- function() {
  strftime_dir <- system.file("www/shared/strftime", package = "shiny")
  if (nzchar(strftime_dir) && dir.exists(strftime_dir)) {
    shiny::addResourcePath("strftime-0.9.2", strftime_dir)
  }
}
