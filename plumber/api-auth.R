# plumber-auth.R

library(plumber)
library(DBI)
library(jsonlite)
library(sodium)
library(jose)

# 🔐 Secret key for signing JWT tokens
secret_key <- Sys.getenv("jws")

# 🔌 Database connection function
get_connection <- function() {
  tryCatch(connect(), error = function(e) {
    message("Unable to connect to database: ", e$message)
    NULL
  })
}

# 🔐 JWT helper function
generate_token <- function(username) {
  payload <- list(
    sub = username,
    iat = as.integer(Sys.time()),
    exp = as.integer(Sys.time()) + 3600  # Token valid for 1 hour
  )
  jwt_encode_hmac(payload, secret_key)
}

#* User login
#* @post /login
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)

  if (is.null(body$username) || is.null(body$password)) {
    res$status <- 400
    return(list(success = FALSE, message = "Missing username or password"))
  }

  con <- get_connection()
  if (is.null(con)) {
    res$status <- 500
    return(list(success = FALSE, message = "Database connection failed"))
  }

  username <- tolower(body$username)
  password <- body$password

  query <- sprintf("SELECT * FROM users WHERE username = '%s'", username)
  user <- DBI::dbGetQuery(con, query)
  DBI::dbDisconnect(con)

  if (nrow(user) == 1 && password_verify(user$password[1], password)) {
    token <- generate_token(username)
    return(list(
      success = TRUE,
      message = "Login successful",
      token = token,
      user = username
    ))
  } else {
    res$status <- 401
    return(list(success = FALSE, message = "Invalid username or password"))
  }
}

#* User registration
#* @post /register
#* @serializer json
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)

  if (is.null(body$username) || is.null(body$password) || is.null(body$email)) {
    res$status <- 400
    return(list(success = FALSE, message = "Missing username, password, or email"))
  }

  con <- get_connection()
  if (is.null(con)) {
    res$status <- 500
    return(list(success = FALSE, message = "Database connection failed"))
  }

  username <- tolower(body$username)
  email <- tolower(body$email)
  hashed_password <- sodium::password_store(body$password)

  query <- sprintf(
    "INSERT INTO users (username, password, email) VALUES ('%s', '%s', '%s')",
    username, hashed_password, email
  )

  tryCatch({
    DBI::dbExecute(con, query)
    DBI::dbDisconnect(con)
    return(list(success = TRUE, message = "Registration successful"))
  }, error = function(e) {
    DBI::dbDisconnect(con)
    res$status <- 400
    return(list(success = FALSE, message = "Registration failed: User may already exist"))
  })
}

#* Logout (stateless placeholder — client deletes token)
#* @post /logout
#* @serializer json
function(req, res) {
  # Optionally clear client-side token (e.g., in JS)
  return(list(success = TRUE, message = "Logged out — client should clear the token"))
}
