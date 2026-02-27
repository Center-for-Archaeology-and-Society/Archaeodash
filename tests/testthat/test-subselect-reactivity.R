test_that("subgroup selection does not reset after user changes it", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app_dir <- normalizePath(testthat::test_path("..", "..", "inst", "app"), mustWork = TRUE)
  csv_path <- normalizePath(testthat::test_path("..", "..", "inst", "app", "INAA_test.csv"), mustWork = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "subselect-reactivity",
    seed = 1
  )
  on.exit(app$stop(), add = TRUE)

  app$upload_file(file1 = csv_path)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "loadcolumns", timeout = 20000)
  app$set_inputs(loadData = "click")
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "attrGroups", timeout = 20000)

  app$set_inputs(attrGroups = "site_name", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  initial_selection <- app$get_value(input = "attrGroupsSub")
  expect_true(length(initial_selection) >= 2)

  new_selection <- unname(initial_selection[1:2])
  app$set_inputs(attrGroupsSub = new_selection)
  app$wait_for_idle(timeout = 20000)

  current_selection <- app$get_value(input = "attrGroupsSub")
  expect_setequal(current_selection, new_selection)

  app$set_inputs(groupDeselectAll = "click")
  app$wait_for_idle(timeout = 20000)
  deselected_values <- app$get_value(input = "attrGroupsSub")
  expect_length(deselected_values, 0)
})
