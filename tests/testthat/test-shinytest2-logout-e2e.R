test_that("logout clears session UI state after authenticated session", {
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
    skip("Database connection unavailable for logout e2e test.")
  }
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  app_dir <- archaeo_test_app_dir()
  temp_id <- paste0(as.integer(Sys.time()), sample(1000:9999, 1))
  username <- paste0("zz_logout_", temp_id)
  password <- paste0("Pw!", temp_id, "x")
  email <- paste0(username, "@example.com")

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "logout-e2e",
    seed = 1
  )
  on.exit(app$stop(), add = TRUE)

  # Ensure remembered-login flow is active for this run.
  app$run_js("document.cookie = 'archaeodash_cookie_consent=accepted; path=/; SameSite=Lax';")
  app$set_inputs(cookie_consent = "accepted")
  app$wait_for_idle(timeout = 10000)

  app$set_inputs(loginUI = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(registerCheckbox = TRUE)
  app$set_inputs(username = username, password = password, email = email, rememberLogin = TRUE)
  app$set_inputs(register = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(registerConfirm = "click")
  app$wait_for_idle(timeout = 30000)

  logout_display_before <- app$get_js("window.getComputedStyle(document.getElementById('logoutUI')).display")
  login_display_before <- app$get_js("window.getComputedStyle(document.getElementById('loginUI')).display")
  expect_identical(as.character(logout_display_before), "inline-block")
  expect_identical(as.character(login_display_before), "none")

  app$set_inputs(logoutUI = "click")
  app$wait_for_idle(timeout = 20000)

  logout_display_after <- app$get_js("window.getComputedStyle(document.getElementById('logoutUI')).display")
  login_display_after <- app$get_js("window.getComputedStyle(document.getElementById('loginUI')).display")
  user_message_after <- app$get_js("(document.getElementById('userMessage') || {}).innerText || ''")
  cookies_after <- app$get_js("document.cookie")

  expect_identical(as.character(logout_display_after), "none")
  expect_identical(as.character(login_display_after), "inline-block")
  expect_identical(as.character(user_message_after), "")
  expect_false(grepl("archaeodash_auth_token=", as.character(cookies_after), fixed = TRUE))
})
