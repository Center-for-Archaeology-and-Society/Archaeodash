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
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("login", "Login"),
        actionButton("register", "Register")
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

  observeEvent(input$login, {
    removeModal()
    username <- tolower(isolate(input$username))
    password <- isolate(input$password)
    # Query to check the user
    query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
    credentials$res <- DBI::dbGetQuery(con, query)
    if (nrow(credentials$res) == 1 && sodium::password_verify(credentials$res$password[1], password)) {
      # Login success
      mynotification("Login Successful")
    } else {
      # Login fail
      mynotification("Login Failed", type = "error")
      credentials$res = tibble::tibble(username = NA)
    }
  })

  observeEvent(input$register, {
    removeModal()
    username <- tolower(isolate(input$username))
    password <- isolate(input$password)
    hashed_password <- sodium::password_store(password)
    # Query to insert new user
    query <- sprintf("INSERT INTO users (username, password) VALUES ('%s', '%s')", username, hashed_password)
    tryCatch({
      DBI::dbExecute(con, query)
      mynotification("Registration Successful")

      query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
      credentials$res <- DBI::dbGetQuery(con, query)
    }, error = function(e) {
      mynotification("Registration Failed: User already exists or invalid data")
      credentials$res = tibble::tibble(username = NA)
    })
  })
  return(credentials)
}

