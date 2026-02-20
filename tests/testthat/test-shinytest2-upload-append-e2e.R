test_that("upload append handles shared column type mismatch without bind_rows failure", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  testthat::skip_on_cran()

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!is.character(chrome_path) || length(chrome_path) == 0 || !nzchar(chrome_path[[1]])) {
    testthat::skip("Chrome/Chromium not available for shinytest2 run.")
  }

  app_dir <- normalizePath(testthat::test_path("..", "..", "inst", "app"), mustWork = TRUE)

  csv1 <- tempfile(fileext = ".csv")
  csv2 <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      ANID = c("A1", "A2"),
      grp = c("G1", "G2"),
      Fe = c(1.1, 2.2),
      Mn = c(10.0, 20.0),
      check.names = FALSE
    ),
    csv1,
    row.names = FALSE
  )
  write.csv(
    data.frame(
      ANID = c("A3", "A4"),
      grp = c("G3", "G4"),
      Fe = c("3.3", "4.4"),
      Mn = c(30.0, 40.0),
      check.names = FALSE
    ),
    csv2,
    row.names = FALSE
  )

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "upload-append-type-mismatch",
    seed = 1
  )
  on.exit(app$stop(), add = TRUE)

  # First load: Fe and Mn both treated as element (numeric) columns.
  app$upload_file(file1 = csv1)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "loadcolumns", timeout = 20000)
  app$set_inputs(loadcolumns = c("ANID", "grp", "Fe", "Mn"))
  app$set_inputs(loadchem = c("Fe", "Mn"))
  app$set_inputs(loadMode = "replace")
  app$set_inputs(loadData = "click")
  app$wait_for_idle(timeout = 30000)
  app$set_inputs(nav = "Explore")
  app$wait_for_idle(timeout = 15000)
  app$wait_for_value(input = "datasetDT_rows_all", timeout = 15000)
  first_rows <- app$get_value(input = "datasetDT_rows_all")
  expect_equal(length(first_rows), 2)

  # Second load: keep Fe as selected column but remove it from loadchem so it enters as character.
  # This reproduces prior bind_rows type mismatch during append.
  app$upload_file(file1 = csv2)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "loadcolumns", timeout = 20000)
  app$set_inputs(loadcolumns = c("ANID", "grp", "Fe", "Mn"))
  app$set_inputs(loadchem = c("Mn"))
  app$set_inputs(loadMode = "add")
  app$set_inputs(loadData = "click")
  app$wait_for_idle(timeout = 30000)

  # Confirm combined dataset exists with all four rows.
  app$set_inputs(nav = "Explore")
  app$wait_for_idle(timeout = 15000)
  app$wait_for_value(input = "datasetDT_rows_all", timeout = 15000)
  appended_rows <- app$get_value(input = "datasetDT_rows_all")
  expect_equal(length(appended_rows), 4)
})
