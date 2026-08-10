source(testthat::test_path("..", "..", "R", "packageChecks.R"))
source(testthat::test_path("..", "..", "R", "appLogging.R"))
source(testthat::test_path("..", "..", "R", "quietly.R"))
source(testthat::test_path("..", "..", "R", "authEmail.R"))
source(testthat::test_path("..", "..", "R", "loginServer.R"))

test_that("remember token generation and hashing behave as expected", {
  skip_if_not_installed("sodium")

  token <- make_remember_token()
  expect_true(is.character(token))
  expect_true(nzchar(token))
  expect_match(token, "^[0-9a-f]+$")

  hash1 <- hash_remember_token(token)
  hash2 <- hash_remember_token(token)
  expect_equal(hash1, hash2)
  expect_match(hash1, "^[0-9a-f]+$")
  expect_false(identical(hash1, hash_remember_token(paste0(token, "x"))))
})

test_that("login rate limit blocks after repeated failures and clears after success", {
  key <- paste0("test|", as.integer(Sys.time()))

  first <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_true(first$allowed)

  record_login_failure(key)
  record_login_failure(key)
  second <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_true(second$allowed)

  record_login_failure(key)
  blocked <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_false(blocked$allowed)
  expect_true(blocked$retry_after >= 1L)

  clear_login_failures(key)
  cleared <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_true(cleared$allowed)
})

test_that("email helper validators behave as expected", {
  expect_true(is_valid_email_address("person@example.com"))
  expect_false(is_valid_email_address("not-an-email"))
  expect_equal(normalize_auth_email(" Person@Example.Com "), "person@example.com")
})

test_that("email_verified_value recognizes verified and unverified rows", {
  expect_false(email_verified_value(data.frame(username = "a", stringsAsFactors = FALSE)))
  expect_false(email_verified_value(data.frame(email_verified_at = NA_character_, stringsAsFactors = FALSE)))
  expect_true(email_verified_value(data.frame(email_verified_at = "2026-05-01 00:00:00", stringsAsFactors = FALSE)))
})

test_that("auth migrations create required tables and add email verification column", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("sodium")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "users",
    data.frame(
      username = character(),
      password = character(),
      email = character(),
      stringsAsFactors = FALSE
    )
  )

  state <- run_auth_migrations(con)
  expect_true(auth_schema_ready(state))

  schema_state <- validate_auth_schema(con)
  expect_true(schema_state$ok)
  expect_true(all(auth_required_tables %in% DBI::dbListTables(con)))
  expect_true("email_verified_at" %in% DBI::dbListFields(con, "users"))
})

test_that("auth schema validation reports missing columns", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "users",
    data.frame(username = character(), stringsAsFactors = FALSE)
  )

  schema_state <- validate_auth_schema(con)
  expect_false(schema_state$ok)
  expect_true(any(grepl("Missing auth table", schema_state$errors)))
  expect_true(any(grepl("users missing column", schema_state$errors)))
})

test_that("registration survives consent modal replacing original inputs", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("sodium")
  skip_if_not_installed("curl")

  old_env <- Sys.getenv(c(
    "ARCHAEODASH_AUTH_EMAIL_MODE",
    "ARCHAEODASH_SMTP_FROM",
    "ARCHAEODASH_BASE_URL"
  ), unset = NA_character_)
  on.exit({
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(as.list(old_env[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.setenv(
    ARCHAEODASH_AUTH_EMAIL_MODE = "log",
    ARCHAEODASH_SMTP_FROM = "noreply@example.com",
    ARCHAEODASH_BASE_URL = "https://archaeodash.example.test/"
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(
    con,
    "users",
    data.frame(
      username = character(),
      password = character(),
      email = character(),
      stringsAsFactors = FALSE
    )
  )

  credentials <- shiny::reactiveValues()
  shiny::testServer(
    function(input, output, session) {
      loginServer(con, input, output, session, credentials)
    },
    {
      session$setInputs(
        username = "NewUser",
        password = "password123",
        email = "NewUser@Example.com"
      )
      session$setInputs(register = 1)
      session$flushReact()

      session$setInputs(username = NULL, password = NULL, email = NULL)
      session$setInputs(registerConfirm = 1)
      session$flushReact()
    }
  )

  users <- DBI::dbReadTable(con, "users")
  expect_equal(nrow(users), 1L)
  expect_equal(users$username[[1]], "newuser")
  expect_equal(users$email[[1]], "newuser@example.com")
  expect_false(email_verified_value(users))
  expect_true(sodium::password_verify(users$password[[1]], "password123"))
})

test_that("registration retry resends verification for matching unverified account", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("sodium")
  skip_if_not_installed("curl")

  old_env <- Sys.getenv(c(
    "ARCHAEODASH_AUTH_EMAIL_MODE",
    "ARCHAEODASH_SMTP_FROM",
    "ARCHAEODASH_BASE_URL"
  ), unset = NA_character_)
  on.exit({
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(as.list(old_env[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.setenv(
    ARCHAEODASH_AUTH_EMAIL_MODE = "log",
    ARCHAEODASH_SMTP_FROM = "noreply@example.com",
    ARCHAEODASH_BASE_URL = "https://archaeodash.example.test/"
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE users (
      username TEXT NOT NULL UNIQUE,
      password TEXT NOT NULL,
      email TEXT NOT NULL,
      email_verified_at TEXT NULL
    )"
  )
  DBI::dbExecute(
    con,
    DBI::sqlInterpolate(
      con,
      "INSERT INTO users (username, password, email, email_verified_at) VALUES (?username, ?password, ?email, NULL)",
      username = "newuser",
      password = sodium::password_store("password123"),
      email = "newuser@example.com"
    )
  )

  credentials <- shiny::reactiveValues()
  shiny::testServer(
    function(input, output, session) {
      loginServer(con, input, output, session, credentials)
    },
    {
      session$setInputs(
        username = "NewUser",
        password = "password123",
        email = "NewUser@Example.com"
      )
      session$setInputs(register = 1)
      session$flushReact()
      session$setInputs(username = NULL, password = NULL, email = NULL)
      session$setInputs(registerConfirm = 1)
      session$flushReact()
    }
  )

  users <- DBI::dbReadTable(con, "users")
  tokens <- DBI::dbReadTable(con, "email_verification_tokens")
  expect_equal(nrow(users), 1L)
  expect_equal(nrow(tokens), 1L)
  expect_equal(tokens$username[[1]], "newuser")
  expect_equal(tokens$email[[1]], "newuser@example.com")
})
