test_that("multiplot no-X warning clears loading state", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  testthat::skip_on_cran()

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!is.character(chrome_path) || length(chrome_path) == 0 || !nzchar(chrome_path[[1]])) {
    testthat::skip("Chrome/Chromium not available for shinytest2 run.")
  }

  app_dir <- normalizePath(testthat::test_path("..", "..", "inst", "app"), mustWork = TRUE)
  csv_path <- normalizePath(testthat::test_path("..", "..", "inst", "app", "INAA_test.csv"), mustWork = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "multiplot-loader-no-x",
    seed = 1
  )
  on.exit(app$stop(), add = TRUE)

  app$upload_file(file1 = csv_path)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "loadcolumns", timeout = 20000)
  app$set_inputs(loadData = "click")
  app$wait_for_idle(timeout = 30000)
  app$set_inputs(chem = c("as", "la", "lu", "nd"))
  app$wait_for_idle(timeout = 8000)
  app$set_inputs(action = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(confirmTransformationAction = "click")
  app$wait_for_idle(timeout = 60000)

  app$set_inputs(nav = "visualizetab")
  app$wait_for_idle(timeout = 15000)
  app$run_js(
    "var tab = Array.from(document.querySelectorAll('a[data-toggle=\"tab\"]')).find(function(a) { return ((a.textContent || '').trim().toLowerCase() === 'multiplots'); }); if (tab) { tab.click(); }"
  )
  app$wait_for_idle(timeout = 8000)

  app$set_inputs(xvar2 = character(0))
  app$wait_for_idle(timeout = 3000)
  app$set_inputs(updateMultiplot = "click")
  app$wait_for_idle(timeout = 8000)

  app$run_js(
    "Shiny.setInputValue('multiplot_no_x_probe', { hasWrap: !!document.getElementById('multiplot-loading-wrap'), modalOpen: document.body.classList.contains('modal-open'), modalCount: document.querySelectorAll('.modal').length, notifText: Array.from(document.querySelectorAll('.shiny-notification')).map(function(n) { return (n.textContent || '').trim(); }) }, { priority: 'event' });"
  )
  app$wait_for_value(input = "multiplot_no_x_probe", timeout = 5000)
  probe <- app$get_value(input = "multiplot_no_x_probe")

  expect_false(isTRUE(probe$hasWrap))
  expect_false(isTRUE(probe$modalOpen))
  expect_equal(as.integer(probe$modalCount), 0L)
  expect_true(length(probe$notifText) >= 1)
  expect_match(
    tolower(paste(as.character(probe$notifText), collapse = " ")),
    "select at least one x variable"
  )
})
