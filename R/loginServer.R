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
    if (!nzchar(username) || !nzchar(password)) {
      mynotification("Login Failed: username and password are required.", type = "error")
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
      if (isTRUE(input$rememberLogin)) {
        session$sendCustomMessage("auth_cookie", list(action = "set", username = username))
      }
      mynotification("Login Successful")
    } else {
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
    if (!nzchar(username) || !nzchar(password)) {
      mynotification("Registration Failed: username and password are required.", type = "error")
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
      if (isTRUE(input$rememberLogin)) {
        session$sendCustomMessage("auth_cookie", list(action = "set", username = username))
      }
    }, error = function(e) {
      mynotification("Registration Failed: User already exists or invalid data")
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
    })
  })

  observeEvent(input$remembered_username, {
    if (!app_require_packages("DBI", feature = "Remembered login")) {
      return(NULL)
    }
    if (is.null(con)) {
      return(NULL)
    }
    if (isTRUE(credentials$status)) {
      return(NULL)
    }
    username <- tolower(trimws(input$remembered_username))
    if (!nzchar(username)) {
      return(NULL)
    }
    remembered_res <- tryCatch(select_user_by_username(username), error = function(e) NULL)
    if (is.data.frame(remembered_res) && nrow(remembered_res) == 1) {
      credentials$res <- remembered_res
      credentials$status <- TRUE
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
      session$sendCustomMessage(
        "auth_cookie",
        list(action = "set", username = as.character(credentials$res$username[[1]]))
      )
    }
    if (isTRUE(input$cookie_consent == "declined")) {
      session$sendCustomMessage("auth_cookie", list(action = "clear"))
    }
  }, ignoreInit = TRUE)

  return(credentials)
}

