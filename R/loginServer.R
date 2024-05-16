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

  observeEvent(input$register, {
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

