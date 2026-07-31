#' loginUI
#'
#' @param input shiny input object
#'
#' @return NULL
#' @export
#'
#' @examples
#' loginUI(input)
loginUI = function(input){
  observeEvent(input$loginUI,{
    app_log("login server")

    showModal(modalDialog(
      title = "Login or Register",
      checkboxInput("registerCheckbox", "Register as a new user", value = FALSE),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      checkboxInput("rememberLogin", "Keep me logged in for 30 days on this device", value = TRUE),
      uiOutput("emailUI"),
      actionLink("forgotPasswordLink", "Forgot your password?"),
      footer = tagList(
        modalButton("Cancel"),
        uiOutput("loginButtonUI")
      )
    ))

  })

}

# In-memory rate-limit registry per R process.
login_rate_limit_registry <- new.env(parent = emptyenv())

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

make_login_rate_limit_key <- function(session, username = "") {
  remote <- tryCatch(as.character(session$request$REMOTE_ADDR), error = function(e) "")
  if (!nzchar(remote)) remote <- "unknown"
  uname <- tolower(trimws(as.character(username)))
  if (!nzchar(uname)) uname <- "unknown"
  paste(remote, uname, sep = "|")
}

check_login_rate_limit <- function(key, max_attempts = 10L, window_seconds = 15L * 60L) {
  now <- as.numeric(Sys.time())
  attempts <- login_rate_limit_registry[[key]]
  if (is.null(attempts)) attempts <- numeric()
  attempts <- attempts[(now - attempts) <= window_seconds]
  login_rate_limit_registry[[key]] <- attempts
  if (length(attempts) >= max_attempts) {
    return(list(allowed = FALSE, retry_after = max(1L, ceiling(window_seconds - (now - min(attempts))))))
  }
  list(allowed = TRUE, retry_after = 0L)
}

record_login_failure <- function(key) {
  now <- as.numeric(Sys.time())
  attempts <- login_rate_limit_registry[[key]]
  if (is.null(attempts)) attempts <- numeric()
  login_rate_limit_registry[[key]] <- c(attempts, now)
  invisible(NULL)
}

clear_login_failures <- function(key) {
  login_rate_limit_registry[[key]] <- numeric()
  invisible(NULL)
}

make_remember_token <- function() {
  if (!app_require_packages("sodium", feature = "Remembered login token generation")) return("")
  sodium::bin2hex(sodium::random(32))
}

hash_remember_token <- function(token) {
  token <- as.character(token)
  if (!nzchar(token)) return("")
  if (!app_require_packages("sodium", feature = "Remembered login token hashing")) return("")
  sodium::bin2hex(sodium::sha256(charToRaw(enc2utf8(token))))
}

normalize_auth_email <- function(email) {
  tolower(trimws(as.character(email)))
}

is_valid_email_address <- function(email) {
  email <- normalize_auth_email(email)
  nzchar(email) && grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", email)
}

email_verified_value <- function(user_row) {
  if (!is.data.frame(user_row) || nrow(user_row) != 1 || !("email_verified_at" %in% names(user_row))) return(FALSE)
  val <- user_row$email_verified_at[[1]]
  !(is.null(val) || is.na(val) || identical(as.character(val), ""))
}

auth_db_is_sqlite <- function(con) {
  inherits(con, "SQLiteConnection")
}

auth_table_sql <- function(con, table_name) {
  DBI::SQL(as.character(DBI::dbQuoteIdentifier(con, table_name)))
}

auth_column_exists <- function(con, table_name, column_name) {
  cols <- tryCatch(DBI::dbListFields(con, table_name), error = function(e) character())
  column_name %in% cols
}

auth_migration_statement <- function(con, name, sql) {
  list(name = name, sql = sql)
}

auth_migration_statements <- function(con) {
  users_tbl <- as.character(DBI::dbQuoteIdentifier(con, "users"))
  remember_tbl <- as.character(DBI::dbQuoteIdentifier(con, "remember_tokens"))
  verify_tbl <- as.character(DBI::dbQuoteIdentifier(con, "email_verification_tokens"))
  reset_tbl <- as.character(DBI::dbQuoteIdentifier(con, "password_reset_tokens"))

  if (auth_db_is_sqlite(con)) {
    return(list(
      auth_migration_statement(
        con,
        "create_remember_tokens",
        paste0(
          "CREATE TABLE IF NOT EXISTS ", remember_tbl, " (",
          "id INTEGER PRIMARY KEY AUTOINCREMENT, ",
          "username TEXT NOT NULL, ",
          "token_hash TEXT NOT NULL UNIQUE, ",
          "created_at TEXT NOT NULL, ",
          "expires_at TEXT NOT NULL, ",
          "last_used_at TEXT NULL, ",
          "revoked INTEGER NOT NULL DEFAULT 0",
          ")"
        )
      ),
      auth_migration_statement(
        con,
        "create_email_verification_tokens",
        paste0(
          "CREATE TABLE IF NOT EXISTS ", verify_tbl, " (",
          "id INTEGER PRIMARY KEY AUTOINCREMENT, ",
          "username TEXT NOT NULL, ",
          "email TEXT NOT NULL, ",
          "token_hash TEXT NOT NULL UNIQUE, ",
          "created_at TEXT NOT NULL, ",
          "expires_at TEXT NOT NULL, ",
          "used_at TEXT NULL, ",
          "revoked INTEGER NOT NULL DEFAULT 0",
          ")"
        )
      ),
      auth_migration_statement(
        con,
        "create_password_reset_tokens",
        paste0(
          "CREATE TABLE IF NOT EXISTS ", reset_tbl, " (",
          "id INTEGER PRIMARY KEY AUTOINCREMENT, ",
          "username TEXT NOT NULL, ",
          "email TEXT NOT NULL, ",
          "token_hash TEXT NOT NULL UNIQUE, ",
          "created_at TEXT NOT NULL, ",
          "expires_at TEXT NOT NULL, ",
          "used_at TEXT NULL, ",
          "revoked INTEGER NOT NULL DEFAULT 0",
          ")"
        )
      )
    ))
  }

  list(
    auth_migration_statement(
      con,
      "create_remember_tokens",
      paste0(
        "CREATE TABLE IF NOT EXISTS ", remember_tbl, " (",
        "id BIGINT AUTO_INCREMENT PRIMARY KEY, ",
        "username VARCHAR(255) NOT NULL, ",
        "token_hash VARCHAR(128) NOT NULL, ",
        "created_at DATETIME NOT NULL, ",
        "expires_at DATETIME NOT NULL, ",
        "last_used_at DATETIME NULL, ",
        "revoked TINYINT(1) NOT NULL DEFAULT 0, ",
        "UNIQUE KEY token_hash_unique (token_hash), ",
        "INDEX idx_username (username), ",
        "INDEX idx_expires (expires_at), ",
        "INDEX idx_revoked (revoked)",
        ")"
      )
    ),
    auth_migration_statement(
      con,
      "create_email_verification_tokens",
      paste0(
        "CREATE TABLE IF NOT EXISTS ", verify_tbl, " (",
        "id BIGINT AUTO_INCREMENT PRIMARY KEY, ",
        "username VARCHAR(255) NOT NULL, ",
        "email VARCHAR(255) NOT NULL, ",
        "token_hash VARCHAR(128) NOT NULL, ",
        "created_at DATETIME NOT NULL, ",
        "expires_at DATETIME NOT NULL, ",
        "used_at DATETIME NULL, ",
        "revoked TINYINT(1) NOT NULL DEFAULT 0, ",
        "UNIQUE KEY token_hash_unique (token_hash), ",
        "INDEX idx_username (username), ",
        "INDEX idx_email (email), ",
        "INDEX idx_expires (expires_at), ",
        "INDEX idx_revoked (revoked)",
        ")"
      )
    ),
    auth_migration_statement(
      con,
      "create_password_reset_tokens",
      paste0(
        "CREATE TABLE IF NOT EXISTS ", reset_tbl, " (",
        "id BIGINT AUTO_INCREMENT PRIMARY KEY, ",
        "username VARCHAR(255) NOT NULL, ",
        "email VARCHAR(255) NOT NULL, ",
        "token_hash VARCHAR(128) NOT NULL, ",
        "created_at DATETIME NOT NULL, ",
        "expires_at DATETIME NOT NULL, ",
        "used_at DATETIME NULL, ",
        "revoked TINYINT(1) NOT NULL DEFAULT 0, ",
        "UNIQUE KEY token_hash_unique (token_hash), ",
        "INDEX idx_username (username), ",
        "INDEX idx_email (email), ",
        "INDEX idx_expires (expires_at), ",
        "INDEX idx_revoked (revoked)",
        ")"
      )
    )
  )
}

auth_required_tables <- c("users", "remember_tokens", "email_verification_tokens", "password_reset_tokens")
auth_required_columns <- list(
  users = c("username", "password", "email", "email_verified_at"),
  remember_tokens = c("id", "username", "token_hash", "created_at", "expires_at", "last_used_at", "revoked"),
  email_verification_tokens = c("id", "username", "email", "token_hash", "created_at", "expires_at", "used_at", "revoked"),
  password_reset_tokens = c("id", "username", "email", "token_hash", "created_at", "expires_at", "used_at", "revoked")
)

validate_auth_schema <- function(con) {
  if (is.null(con)) return(list(ok = FALSE, errors = "Database connection is unavailable."))
  missing_tables <- auth_required_tables[!vapply(
    auth_required_tables,
    function(table_name) DBI::dbExistsTable(con, table_name),
    logical(1)
  )]
  errors <- character()
  if (length(missing_tables) > 0) {
    errors <- c(errors, paste0("Missing auth table(s): ", paste(missing_tables, collapse = ", ")))
  }

  existing_tables <- setdiff(auth_required_tables, missing_tables)
  for (table_name in existing_tables) {
    fields <- tryCatch(DBI::dbListFields(con, table_name), error = function(e) character())
    missing_cols <- setdiff(auth_required_columns[[table_name]], fields)
    if (length(missing_cols) > 0) {
      errors <- c(errors, paste0("Table ", table_name, " missing column(s): ", paste(missing_cols, collapse = ", ")))
    }
  }

  list(ok = length(errors) == 0, errors = errors)
}

run_auth_migrations <- function(con) {
  if (!app_require_packages(c("DBI", "sodium"), feature = "Authentication")) {
    return(list(ok = FALSE, errors = "Required authentication packages are unavailable."))
  }
  if (is.null(con)) return(list(ok = FALSE, errors = "Database connection is unavailable."))

  results <- list()
  errors <- character()
  for (statement in auth_migration_statements(con)) {
    ok <- tryCatch({
      DBI::dbExecute(con, statement$sql)
      TRUE
    }, error = function(e) {
      errors <<- c(errors, paste0(statement$name, ": ", conditionMessage(e)))
      FALSE
    })
    app_log(paste0("Auth migration ", statement$name, ": ", if (isTRUE(ok)) "ok" else "failed"))
    results[[statement$name]] <- ok
  }

  if (!auth_column_exists(con, "users", "email_verified_at")) {
    users_tbl <- as.character(DBI::dbQuoteIdentifier(con, "users"))
    alter_sql <- paste0("ALTER TABLE ", users_tbl, " ADD COLUMN email_verified_at ", if (auth_db_is_sqlite(con)) "TEXT NULL" else "DATETIME NULL")
    ok <- tryCatch({
      DBI::dbExecute(con, alter_sql)
      TRUE
    }, error = function(e) {
      errors <<- c(errors, paste0("add_users_email_verified_at: ", conditionMessage(e)))
      FALSE
    })
    app_log(paste0("Auth migration add_users_email_verified_at: ", if (isTRUE(ok)) "ok" else "failed"))
    results[["add_users_email_verified_at"]] <- ok
  } else {
    app_log("Auth migration add_users_email_verified_at: already present")
    results[["add_users_email_verified_at"]] <- TRUE
  }

  schema_state <- validate_auth_schema(con)
  if (!isTRUE(schema_state$ok)) {
    errors <- c(errors, schema_state$errors)
  }
  list(ok = length(errors) == 0, errors = unique(errors), results = results)
}

auth_schema_ready <- function(state) {
  is.list(state) && isTRUE(state$ok)
}

auth_unavailable_message <- function() {
  "Authentication is temporarily unavailable. Please contact support if this persists."
}

#' loginServer
#'
#' @param con Database connection
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param credentials reactive values object
#'
#' @return credentials object
#' @export
#'
#' @examples
#' loginServer(con,input,output,session,credentials)
loginServer = function(con, input = input, output = output, session = session, credentials = credentials){

  users_table_sql <- function() {
    DBI::SQL(as.character(DBI::dbQuoteIdentifier(con, "users")))
  }
  remember_tokens_table_sql <- function() {
    DBI::SQL(as.character(DBI::dbQuoteIdentifier(con, "remember_tokens")))
  }
  email_verification_tokens_table_sql <- function() {
    DBI::SQL(as.character(DBI::dbQuoteIdentifier(con, "email_verification_tokens")))
  }
  password_reset_tokens_table_sql <- function() {
    DBI::SQL(as.character(DBI::dbQuoteIdentifier(con, "password_reset_tokens")))
  }

  ensure_auth_tables <- function() {
    state <- run_auth_migrations(con)
    if (!auth_schema_ready(state)) {
      app_log(paste0("Auth migration failed: ", paste(state$errors, collapse = " | ")))
    }
    state
  }

  require_auth_schema <- function(notify = TRUE) {
    if (!auth_schema_ready(credentials$auth_schema)) {
      if (isTRUE(notify)) mynotification(auth_unavailable_message(), type = "error")
      return(FALSE)
    }
    TRUE
  }

  now_chr <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  select_user_by_username <- function(username) {
    query <- DBI::sqlInterpolate(
      con,
      "SELECT * FROM ?users_tbl WHERE username = ?username",
      users_tbl = users_table_sql(),
      username = username
    )
    DBI::dbGetQuery(con, query)
  }

  select_user_by_email <- function(email) {
    query <- DBI::sqlInterpolate(
      con,
      "SELECT * FROM ?users_tbl WHERE LOWER(email) = ?email ORDER BY username LIMIT 1",
      users_tbl = users_table_sql(),
      email = normalize_auth_email(email)
    )
    DBI::dbGetQuery(con, query)
  }

  revoke_remember_token <- function(token) {
    token_hash <- hash_remember_token(token)
    if (!nzchar(token_hash) || is.null(con)) return(invisible(FALSE))
    if (!require_auth_schema(notify = FALSE)) return(invisible(FALSE))
    query <- DBI::sqlInterpolate(
      con,
      "UPDATE ?tbl SET revoked = 1 WHERE token_hash = ?token_hash",
      tbl = remember_tokens_table_sql(),
      token_hash = token_hash
    )
    tryCatch({
      DBI::dbExecute(con, query)
      TRUE
    }, error = function(e) FALSE)
  }

  revoke_known_remember_tokens <- function(...) {
    raw_tokens <- unlist(list(...), use.names = FALSE)
    if (length(raw_tokens) == 0) return(invisible(FALSE))
    tokens <- as.character(raw_tokens)
    tokens <- unique(tokens[!is.na(tokens) & nzchar(tokens)])
    if (length(tokens) == 0) return(invisible(FALSE))
    results <- vapply(tokens, function(tok) isTRUE(try(revoke_remember_token(tok), silent = TRUE)), logical(1))
    invisible(any(results))
  }

  issue_remember_token <- function(username, days_valid = 30L) {
    username <- tolower(trimws(as.character(username)))
    if (!nzchar(username) || is.null(con)) return("")
    if (!require_auth_schema(notify = FALSE)) return("")

    token <- make_remember_token()
    token_hash <- hash_remember_token(token)
    if (!nzchar(token) || !nzchar(token_hash)) return("")

    exp_chr <- format(Sys.time() + as.numeric(days_valid) * 24 * 60 * 60, "%Y-%m-%d %H:%M:%S")
    tryCatch({
      revoke_query <- DBI::sqlInterpolate(
        con,
        "UPDATE ?tbl SET revoked = 1 WHERE username = ?username AND revoked = 0",
        tbl = remember_tokens_table_sql(),
        username = username
      )
      DBI::dbExecute(con, revoke_query)

      insert_query <- DBI::sqlInterpolate(
        con,
        paste0(
          "INSERT INTO ?tbl (username, token_hash, created_at, expires_at, last_used_at, revoked) ",
          "VALUES (?username, ?token_hash, ?created_at, ?expires_at, ?last_used_at, 0)"
        ),
        tbl = remember_tokens_table_sql(),
        username = username,
        token_hash = token_hash,
        created_at = now_chr(),
        expires_at = exp_chr,
        last_used_at = now_chr()
      )
      DBI::dbExecute(con, insert_query)
      token
    }, error = function(e) "")
  }

  find_username_by_remember_token <- function(token) {
    token_hash <- hash_remember_token(token)
    if (!nzchar(token_hash) || is.null(con)) return("")
    if (!require_auth_schema(notify = FALSE)) return("")
    query <- DBI::sqlInterpolate(
      con,
      paste0(
        "SELECT username FROM ?tbl ",
        "WHERE token_hash = ?token_hash AND revoked = 0 AND expires_at > ?now ",
        "ORDER BY id DESC LIMIT 1"
      ),
      tbl = remember_tokens_table_sql(),
      token_hash = token_hash,
      now = now_chr()
    )
    res <- tryCatch(DBI::dbGetQuery(con, query), error = function(e) tibble::tibble())
    if (!inherits(res, "data.frame") || nrow(res) == 0 || !("username" %in% names(res))) return("")
    uname <- as.character(res$username[[1]])
    if (is.na(uname) || !nzchar(uname)) return("")
    uname
  }

  send_remember_token_to_client <- function(username) {
    token <- issue_remember_token(username)
    if (!nzchar(token)) return(invisible(FALSE))
    session$sendCustomMessage("auth_cookie", list(action = "set", token = token))
    invisible(TRUE)
  }

  revoke_auth_tokens_for_user <- function(username, table_sql) {
    username <- tolower(trimws(as.character(username)))
    if (!nzchar(username) || is.null(con)) return(invisible(FALSE))
    query <- DBI::sqlInterpolate(
      con,
      "UPDATE ?tbl SET revoked = 1 WHERE username = ?username AND revoked = 0",
      tbl = table_sql,
      username = username
    )
    tryCatch({
      DBI::dbExecute(con, query)
      TRUE
    }, error = function(e) FALSE)
  }

  issue_auth_token <- function(username, email, table_sql, days_valid = 1L) {
    username <- tolower(trimws(as.character(username)))
    email <- normalize_auth_email(email)
    if (!nzchar(username) || !nzchar(email) || is.null(con)) return("")
    if (!require_auth_schema(notify = FALSE)) return("")

    token <- make_remember_token()
    token_hash <- hash_remember_token(token)
    if (!nzchar(token_hash)) return("")

    exp_chr <- format(Sys.time() + as.numeric(days_valid) * 24 * 60 * 60, "%Y-%m-%d %H:%M:%S")
    tryCatch({
      revoke_auth_tokens_for_user(username, table_sql)
      insert_query <- DBI::sqlInterpolate(
        con,
        paste0(
          "INSERT INTO ?tbl (username, email, token_hash, created_at, expires_at, used_at, revoked) ",
          "VALUES (?username, ?email, ?token_hash, ?created_at, ?expires_at, NULL, 0)"
        ),
        tbl = table_sql,
        username = username,
        email = email,
        token_hash = token_hash,
        created_at = now_chr(),
        expires_at = exp_chr
      )
      DBI::dbExecute(con, insert_query)
      token
    }, error = function(e) "")
  }

  consume_auth_token <- function(token, table_sql) {
    token_hash <- hash_remember_token(token)
    if (!nzchar(token_hash) || is.null(con)) return(tibble::tibble())
    if (!require_auth_schema(notify = FALSE)) return(tibble::tibble())
    query <- DBI::sqlInterpolate(
      con,
      paste0(
        "SELECT * FROM ?tbl WHERE token_hash = ?token_hash ",
        "AND revoked = 0 AND used_at IS NULL AND expires_at > ?now ",
        "ORDER BY id DESC LIMIT 1"
      ),
      tbl = table_sql,
      token_hash = token_hash,
      now = now_chr()
    )
    res <- tryCatch(DBI::dbGetQuery(con, query), error = function(e) tibble::tibble())
    if (!inherits(res, "data.frame") || nrow(res) != 1) return(tibble::tibble())

    update_query <- DBI::sqlInterpolate(
      con,
      "UPDATE ?tbl SET used_at = ?used_at, revoked = 1 WHERE id = ?id",
      tbl = table_sql,
      used_at = now_chr(),
      id = as.integer(res$id[[1]])
    )
    ok <- tryCatch({
      DBI::dbExecute(con, update_query)
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(ok)) return(tibble::tibble())
    res
  }

  send_verification_email <- function(username, email) {
    token <- issue_auth_token(username, email, email_verification_tokens_table_sql(), days_valid = 1L)
    if (!nzchar(token)) return(FALSE)
    verify_url <- build_auth_absolute_url(session, query = list(verify = token))
    if (!nzchar(verify_url)) return(FALSE)
    subject <- auth_email_subject("verify")
    text_body <- paste(
      "Welcome to ArchaeoDash.",
      "",
      "Use the link below to verify your email address and activate your account:",
      verify_url,
      "",
      "This link expires in 24 hours.",
      sep = "\n"
    )
    html_body <- paste0(
      "<p>Welcome to ArchaeoDash.</p>",
      "<p>Use the link below to verify your email address and activate your account:</p>",
      "<p><a href=\"", verify_url, "\">Verify my email</a></p>",
      "<p>This link expires in 24 hours.</p>"
    )
    send_auth_email(email, subject, html_body, text_body)
  }

  send_password_reset_email <- function(username, email) {
    token <- issue_auth_token(username, email, password_reset_tokens_table_sql(), days_valid = 1L)
    if (!nzchar(token)) return(FALSE)
    reset_url <- build_auth_absolute_url(session, query = list(reset = token))
    if (!nzchar(reset_url)) return(FALSE)
    subject <- auth_email_subject("reset")
    text_body <- paste(
      "A password reset was requested for your ArchaeoDash account.",
      "",
      "Use the link below to choose a new password:",
      reset_url,
      "",
      "This link expires in 24 hours.",
      sep = "\n"
    )
    html_body <- paste0(
      "<p>A password reset was requested for your ArchaeoDash account.</p>",
      "<p>Use the link below to choose a new password:</p>",
      "<p><a href=\"", reset_url, "\">Reset my password</a></p>",
      "<p>This link expires in 24 hours.</p>"
    )
    send_auth_email(email, subject, html_body, text_body)
  }

  activate_verified_user <- function(username) {
    username <- tolower(trimws(as.character(username)))
    if (!nzchar(username) || is.null(con)) return(FALSE)
    query <- DBI::sqlInterpolate(
      con,
      "UPDATE ?users_tbl SET email_verified_at = ?verified_at WHERE username = ?username",
      users_tbl = users_table_sql(),
      verified_at = now_chr(),
      username = username
    )
    tryCatch({
      DBI::dbExecute(con, query)
      TRUE
    }, error = function(e) FALSE)
  }

  set_user_password <- function(username, password) {
    username <- tolower(trimws(as.character(username)))
    if (!nzchar(username) || !nzchar(password) || is.null(con)) return(FALSE)
    hashed_password <- sodium::password_store(password)
    query <- DBI::sqlInterpolate(
      con,
      "UPDATE ?users_tbl SET password = ?password WHERE username = ?username",
      users_tbl = users_table_sql(),
      password = hashed_password,
      username = username
    )
    tryCatch({
      DBI::dbExecute(con, query)
      TRUE
    }, error = function(e) FALSE)
  }

  parsed_query <- reactive({
    shiny::parseQueryString(isolate(session$clientData$url_search %||% ""))
  })

  credentials$status = FALSE
  credentials$res = tibble::tibble(username = NA)
  credentials$pending_reset_token <- ""
  credentials$auth_schema <- ensure_auth_tables()

  output$emailUI = renderUI({
    if (input$registerCheckbox) {
      textInput("email", "Email")
    }
  })

  output$loginButtonUI = renderUI({
    if (input$registerCheckbox) {
      actionButton("register", "Register")
    } else {
      actionButton("login", "Login")
    }
  })

  observeEvent(input$forgotPasswordLink, {
    removeModal()
    showModal(modalDialog(
      title = "Reset Password",
      textInput("resetPasswordEmail", "Account email"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("requestPasswordReset", "Send reset link")
      )
    ))
  })

  observeEvent(input$requestPasswordReset, {
    if (!app_require_packages(c("DBI", "sodium"), feature = "Password reset")) return(NULL)
    if (!require_auth_schema()) return(NULL)
    removeModal()
    email <- normalize_auth_email(isolate(input$resetPasswordEmail))
    if (is_valid_email_address(email)) {
      user_row <- tryCatch(select_user_by_email(email), error = function(e) tibble::tibble())
      if (is.data.frame(user_row) && nrow(user_row) == 1 && email_verified_value(user_row)) {
        try(send_password_reset_email(as.character(user_row$username[[1]]), email), silent = TRUE)
      }
    }
    mynotification("If that email matches a verified account, a password reset link has been sent.")
  })

  observeEvent(input$submitResetPassword, {
    if (!app_require_packages(c("DBI", "sodium"), feature = "Password reset")) return(NULL)
    if (!require_auth_schema()) return(NULL)
    token <- as.character(credentials$pending_reset_token %||% "")
    new_password <- isolate(input$resetNewPassword)
    confirm_password <- isolate(input$resetConfirmPassword)
    if (!nzchar(token)) {
      mynotification("Password reset link is missing or expired.", type = "error")
      return(NULL)
    }
    if (!nzchar(new_password) || nchar(new_password) < 8) {
      mynotification("Choose a password with at least 8 characters.", type = "error")
      return(NULL)
    }
    if (!identical(new_password, confirm_password)) {
      mynotification("The new password and confirmation do not match.", type = "error")
      return(NULL)
    }

    token_row <- consume_auth_token(token, password_reset_tokens_table_sql())
    if (!is.data.frame(token_row) || nrow(token_row) != 1) {
      mynotification("Password reset link is invalid or expired.", type = "error")
      credentials$pending_reset_token <- ""
      return(NULL)
    }

    username <- as.character(token_row$username[[1]])
    if (!isTRUE(set_user_password(username, new_password))) {
      mynotification("Password reset failed. Please request a new reset link.", type = "error")
      return(NULL)
    }

    revoke_auth_tokens_for_user(username, password_reset_tokens_table_sql())
    credentials$pending_reset_token <- ""
    removeModal()
    mynotification("Password updated. You can now log in with your new password.")
  })

  observeEvent(parsed_query(), {
    if (!app_require_packages(c("DBI", "sodium"), feature = "Authentication links")) return(NULL)
    if (!require_auth_schema()) return(NULL)
    params <- parsed_query()

    verify_token <- as.character(params$verify %||% "")
    if (nzchar(verify_token)) {
      token_row <- consume_auth_token(verify_token, email_verification_tokens_table_sql())
      if (is.data.frame(token_row) && nrow(token_row) == 1) {
        username <- as.character(token_row$username[[1]])
        if (isTRUE(activate_verified_user(username))) {
          mynotification("Email verified. Your account is ready to use.")
        } else {
          mynotification("Email verification failed. Please request a new verification email.", type = "error")
        }
      } else {
        mynotification("Verification link is invalid or expired.", type = "error")
      }
    }

    reset_token <- as.character(params$reset %||% "")
    if (nzchar(reset_token)) {
      credentials$pending_reset_token <- reset_token
      showModal(modalDialog(
        title = "Choose a New Password",
        passwordInput("resetNewPassword", "New password"),
        passwordInput("resetConfirmPassword", "Confirm new password"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("submitResetPassword", "Update password")
        )
      ))
    }
  }, once = TRUE, ignoreInit = FALSE)

  observeEvent(input$login, {
    if (!app_require_packages(c("DBI", "sodium"), feature = "Login")) {
      return(NULL)
    }
    if (!require_auth_schema()) return(NULL)
    if (is.null(con)) {
      mynotification("Database connection is unavailable. Login is disabled.", type = "error")
      return(NULL)
    }
    removeModal()
    username <- tolower(trimws(isolate(input$username)))
    password <- isolate(input$password)
    rate_key <- make_login_rate_limit_key(session, username)
    limit_state <- check_login_rate_limit(rate_key)
    if (!isTRUE(limit_state$allowed)) {
      mynotification(
        paste0("Too many login attempts. Please wait ", limit_state$retry_after, " seconds and try again."),
        type = "error"
      )
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      return(NULL)
    }
    if (!nzchar(username) || !nzchar(password)) {
      mynotification("Login Failed: username and password are required.", type = "error")
      record_login_failure(rate_key)
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      return(NULL)
    }
    credentials$res <- tryCatch(
      select_user_by_username(username),
      error = function(e) {
        mynotification(paste0("Login failed due to database query error: ", e$message), type = "error")
        tibble::tibble()
      }
    )
    if (nrow(credentials$res) == 1 && sodium::password_verify(credentials$res$password[1], password)) {
      if (!email_verified_value(credentials$res)) {
        clear_login_failures(rate_key)
        try(send_verification_email(username, as.character(credentials$res$email[[1]])), silent = TRUE)
        credentials$res = tibble::tibble(username = NA)
        credentials$status = FALSE
        mynotification("Verify your email before logging in. A fresh verification link has been sent.")
        return(NULL)
      }

      credentials$status = TRUE
      clear_login_failures(rate_key)
      if (isTRUE(input$rememberLogin)) {
        send_remember_token_to_client(username)
      }
      mynotification("Login Successful")
    } else {
      record_login_failure(rate_key)
      mynotification("Login Failed: invalid username or password.", type = "error")
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
    }
  })

  observeEvent(input$register,{
    removeModal()
    showModal(modalDialog(
      title = "Confirmation",
      HTML(" <h3>Notice of Communication Consent</h3>
    <p>
        By registering for an account, you consent to receive emails from us regarding:
    </p>
    <ul>
        <li><strong>Account Updates:</strong> Information and notifications about your account activity, security updates, and changes to your account settings.</li>
        <li><strong>App Updates:</strong> Announcements about new features, improvements, and changes to our application.</li>
        <li><strong>Changes in Terms:</strong> Important updates and modifications to our terms of service, privacy policy, and other legal agreements.</li>
    </ul>
    <p>
        Your consent is necessary to ensure you are kept informed about important aspects of your account and the services we provide. You may manage your email preferences or unsubscribe from certain communications at any time through your account settings.
    </p>"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("registerConfirm", "I Agree")
      )
    ))
  })

  observeEvent(input$registerConfirm, {
    if (!app_require_packages(c("DBI", "sodium"), feature = "Registration")) {
      return(NULL)
    }
    if (!require_auth_schema()) return(NULL)
    if (is.null(con)) {
      mynotification("Database connection is unavailable. Registration is disabled.", type = "error")
      return(NULL)
    }
    removeModal()
    username <- tolower(trimws(isolate(input$username)))
    password <- isolate(input$password)
    email <- normalize_auth_email(isolate(input$email))
    rate_key <- make_login_rate_limit_key(session, username)
    limit_state <- check_login_rate_limit(rate_key)
    if (!isTRUE(limit_state$allowed)) {
      mynotification(
        paste0("Too many registration attempts. Please wait ", limit_state$retry_after, " seconds and try again."),
        type = "error"
      )
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      return(NULL)
    }
    if (!nzchar(username) || !nzchar(password)) {
      mynotification("Registration Failed: username and password are required.", type = "error")
      record_login_failure(rate_key)
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      return(NULL)
    }
    if (!is_valid_email_address(email)) {
      mynotification("Registration Failed: a valid email address is required.", type = "error")
      record_login_failure(rate_key)
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      return(NULL)
    }
    if (nchar(password) < 8) {
      mynotification("Registration Failed: password must be at least 8 characters.", type = "error")
      record_login_failure(rate_key)
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      return(NULL)
    }

    hashed_password <- sodium::password_store(password)
    insert_query <- DBI::sqlInterpolate(
      con,
      paste0(
        "INSERT INTO ?users_tbl (username, password, email, email_verified_at) ",
        "VALUES (?username, ?password, ?email, NULL)"
      ),
      users_tbl = users_table_sql(),
      username = username,
      password = hashed_password,
      email = email
    )
    tryCatch({
      DBI::dbExecute(con, insert_query)
      sent <- isTRUE(send_verification_email(username, email))
      clear_login_failures(rate_key)
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
      if (sent) {
        mynotification("Registration successful. Check your email to verify your account before logging in.")
      } else {
        mynotification("Registration created your account, but verification email delivery failed. Contact support to verify your email.", type = "warning")
      }
    }, error = function(e) {
      record_login_failure(rate_key)
      mynotification("Registration Failed: User already exists or invalid data")
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
    })
  })

  observeEvent(input$remembered_token, {
    if (!app_require_packages("DBI", feature = "Remembered login")) {
      return(NULL)
    }
    if (!require_auth_schema(notify = FALSE)) return(NULL)
    if (is.null(con)) {
      return(NULL)
    }
    if (isTRUE(credentials$status)) {
      return(NULL)
    }
    username <- tolower(trimws(find_username_by_remember_token(input$remembered_token)))
    if (!nzchar(username)) {
      session$sendCustomMessage("auth_cookie", list(action = "clear"))
      return(NULL)
    }
    remembered_res <- tryCatch(select_user_by_username(username), error = function(e) NULL)
    if (is.data.frame(remembered_res) && nrow(remembered_res) == 1 && email_verified_value(remembered_res)) {
      credentials$res <- remembered_res
      credentials$status <- TRUE
      send_remember_token_to_client(username)
      mynotification("Logged in from saved session")
    } else {
      session$sendCustomMessage("auth_cookie", list(action = "clear"))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$cookie_consent, {
    if (isTRUE(input$cookie_consent == "accepted") &&
        isTRUE(credentials$status) &&
        !is.null(credentials$res$username) &&
        !is.na(credentials$res$username)) {
      send_remember_token_to_client(as.character(credentials$res$username[[1]]))
    }
    if (isTRUE(input$cookie_consent == "declined")) {
      revoke_started <- Sys.time()
      revoked <- isTRUE(revoke_known_remember_tokens(input$remembered_token_revoke, input$remembered_token))
      app_timing_log(
        "remember_token_revoke",
        list(
          trigger = "cookie_consent_declined",
          revoked = revoked,
          elapsed_ms = timing_elapsed_ms(revoke_started)
        )
      )
      session$sendCustomMessage("auth_cookie", list(action = "clear"))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$remembered_token_revoke, {
    revoke_started <- Sys.time()
    revoked <- isTRUE(revoke_known_remember_tokens(input$remembered_token_revoke))
    app_timing_log(
      "remember_token_revoke",
      list(
        trigger = "remembered_token_revoke",
        revoked = revoked,
        elapsed_ms = timing_elapsed_ms(revoke_started)
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$logoutUI, {
    revoke_started <- Sys.time()
    revoked <- isTRUE(revoke_known_remember_tokens(input$remembered_token_revoke, input$remembered_token))
    app_timing_log(
      "remember_token_revoke",
      list(
        trigger = "logoutUI",
        revoked = revoked,
        elapsed_ms = timing_elapsed_ms(revoke_started)
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$logout_click_js, {
    revoke_started <- Sys.time()
    revoked <- isTRUE(revoke_known_remember_tokens(input$remembered_token_revoke, input$remembered_token))
    app_timing_log(
      "remember_token_revoke",
      list(
        trigger = "logout_click_js",
        revoked = revoked,
        elapsed_ms = timing_elapsed_ms(revoke_started)
      )
    )
  }, ignoreInit = TRUE)

  observeEvent(input$resetSessionUI, {
    revoke_started <- Sys.time()
    revoked <- isTRUE(revoke_known_remember_tokens(input$remembered_token_revoke, input$remembered_token))
    app_timing_log(
      "remember_token_revoke",
      list(
        trigger = "resetSessionUI",
        revoked = revoked,
        elapsed_ms = timing_elapsed_ms(revoke_started)
      )
    )
  }, ignoreInit = TRUE)

  return(credentials)
}
