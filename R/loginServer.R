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
      p("Forgot password? Email rbischoff@asu.edu for assistance."),
      footer = tagList(
        modalButton("Cancel"),
        uiOutput("loginButtonUI")
      )
    ))

  })

}

# In-memory rate-limit registry per R process.
login_rate_limit_registry <- new.env(parent = emptyenv())

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

  ensure_remember_tokens_table <- function() {
    if (!app_require_packages("DBI", feature = "Remembered login tokens")) return(invisible(NULL))
    if (is.null(con)) return(invisible(NULL))
    tbl_sql <- as.character(DBI::dbQuoteIdentifier(con, "remember_tokens"))
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE IF NOT EXISTS ", tbl_sql, " (",
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
    )
    invisible(NULL)
  }

  revoke_remember_token <- function(token) {
    token_hash <- hash_remember_token(token)
    if (!nzchar(token_hash) || is.null(con)) return(invisible(FALSE))
    ensure_remember_tokens_table()
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

  issue_remember_token <- function(username, days_valid = 30L) {
    username <- tolower(trimws(as.character(username)))
    if (!nzchar(username) || is.null(con)) return("")
    ensure_remember_tokens_table()

    token <- make_remember_token()
    token_hash <- hash_remember_token(token)
    if (!nzchar(token) || !nzchar(token_hash)) return("")

    now_chr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
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
        created_at = now_chr,
        expires_at = exp_chr,
        last_used_at = now_chr
      )
      DBI::dbExecute(con, insert_query)
      token
    }, error = function(e) "")
  }

  find_username_by_remember_token <- function(token) {
    token_hash <- hash_remember_token(token)
    if (!nzchar(token_hash) || is.null(con)) return("")
    ensure_remember_tokens_table()
    now_chr <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    query <- DBI::sqlInterpolate(
      con,
      paste0(
        "SELECT username FROM ?tbl ",
        "WHERE token_hash = ?token_hash AND revoked = 0 AND expires_at > ?now ",
        "ORDER BY id DESC LIMIT 1"
      ),
      tbl = remember_tokens_table_sql(),
      token_hash = token_hash,
      now = now_chr
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

  select_user_by_username <- function(username) {
    query <- DBI::sqlInterpolate(
      con,
      "SELECT * FROM ?users_tbl WHERE username = ?username",
      users_tbl = users_table_sql(),
      username = username
    )
    DBI::dbGetQuery(con, query)
  }

  credentials$status = FALSE
  credentials$res = tibble::tibble(username = NA)

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

  observeEvent(input$login, {
    if (!app_require_packages(c("DBI", "sodium"), feature = "Login")) {
      return(NULL)
    }
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
    if (is.null(con)) {
      mynotification("Database connection is unavailable. Registration is disabled.", type = "error")
      return(NULL)
    }
    removeModal()
    username <- tolower(trimws(isolate(input$username)))
    password <- isolate(input$password)
    email <- tolower(isolate(input$email))
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
    hashed_password <- sodium::password_store(password)
    # Query to insert new user
    insert_query <- DBI::sqlInterpolate(
      con,
      "INSERT INTO ?users_tbl (username, password, email) VALUES (?username, ?password, ?email)",
      users_tbl = users_table_sql(),
      username = username,
      password = hashed_password,
      email = email
    )
    tryCatch({
      DBI::dbExecute(con, insert_query)
      mynotification("Registration Successful")

      credentials$res <- select_user_by_username(username)
      credentials$status = TRUE
      clear_login_failures(rate_key)
      if (isTRUE(input$rememberLogin)) {
        send_remember_token_to_client(username)
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
    if (is.data.frame(remembered_res) && nrow(remembered_res) == 1) {
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
      try(revoke_remember_token(input$remembered_token_revoke), silent = TRUE)
      session$sendCustomMessage("auth_cookie", list(action = "clear"))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$logoutUI, {
    try(revoke_remember_token(input$remembered_token_revoke), silent = TRUE)
  }, ignoreInit = TRUE)

  return(credentials)
}

