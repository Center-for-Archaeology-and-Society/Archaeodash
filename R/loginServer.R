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
    message("login server")

    showModal(modalDialog(
      title = "Login or Register",
      checkboxInput("registerCheckbox", "Register as a new user", value = FALSE),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
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
    username <- tolower(isolate(input$username))
    password <- isolate(input$password)
    query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
    credentials$res <- DBI::dbGetQuery(con, query)
    if (nrow(credentials$res) == 1 && sodium::password_verify(credentials$res$password[1], password)) {
      credentials$status = TRUE
      mynotification("Login Successful")
    } else {
      mynotification("Login Failed", type = "error")
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
    username <- tolower(isolate(input$username))
    password <- isolate(input$password)
    email <- tolower(isolate(input$email))
    hashed_password <- sodium::password_store(password)
    # Query to insert new user
    query <- sprintf("INSERT INTO users (username, password, email) VALUES ('%s', '%s','%s')", username, hashed_password, email)
    tryCatch({
      DBI::dbExecute(con, query)
      mynotification("Registration Successful")

      query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
      credentials$res <- DBI::dbGetQuery(con, query)
      credentials$status = TRUE
    }, error = function(e) {
      mynotification("Registration Failed: User already exists or invalid data")
      credentials$res = tibble::tibble(username = NA)
      credentials$status = FALSE
    })
  })
  return(credentials)
}

