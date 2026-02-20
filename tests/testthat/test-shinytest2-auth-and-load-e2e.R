test_that("register temp user and load INAA_test.csv end-to-end", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("DBI")
  skip_on_cran()
  if (!identical(Sys.getenv("ARCHAEODASH_RUN_E2E"), "1")) {
    skip("Set ARCHAEODASH_RUN_E2E=1 to run DB-backed e2e tests.")
  }

  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!is.character(chrome_path) || length(chrome_path) == 0 || !nzchar(chrome_path[[1]])) {
    skip("Chrome/Chromium not available for shinytest2 run.")
  }

  con <- tryCatch(connect(), error = function(e) NULL)
  if (is.null(con)) {
    skip("Database connection unavailable for registration e2e test.")
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  app_dir <- normalizePath(testthat::test_path("..", "..", "inst", "app"), mustWork = TRUE)
  csv_path <- normalizePath(testthat::test_path("..", "..", "inst", "app", "INAA_test.csv"), mustWork = TRUE)
  temp_id <- paste0(as.integer(Sys.time()), sample(1000:9999, 1))
  username <- paste0("zz_e2e_", temp_id)
  password <- paste0("Pw!", temp_id, "x")
  email <- paste0(username, "@example.com")

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "auth-load-e2e",
    seed = 1
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(loginUI = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(registerCheckbox = TRUE)
  app$set_inputs(username = username, password = password, email = email, rememberLogin = TRUE)
  app$set_inputs(register = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(registerConfirm = "click")
  app$wait_for_idle(timeout = 30000)

  user_msg <- app$run_js("return (document.getElementById('userMessage') || {}).innerText || '';")
  expect_true(grepl(tolower(username), tolower(as.character(user_msg)), fixed = TRUE))

  app$upload_file(file1 = csv_path)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "loadcolumns", timeout = 20000)
  app$set_inputs(loadData = "click")
  app$wait_for_idle(timeout = 30000)
  app$wait_for_value(input = "attrGroups", timeout = 30000)
  loaded_group <- app$get_value(input = "attrGroups")
  expect_true(is.character(loaded_group) || is.factor(loaded_group))
  expect_true(length(loaded_group) >= 1)

  app$set_inputs(action = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(confirmTransformationAction = "click")
  app$wait_for_idle(timeout = 60000)

  selected_nav <- app$get_value(input = "nav")
  expect_true(is.character(selected_nav))
})
