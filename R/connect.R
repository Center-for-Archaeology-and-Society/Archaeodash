
#' Connect
#'
#' @param db database name
#' @param host IP or URL
#' @param user username
#' @param pwd  password
#' @param port port number
#'
#' @return database connection
#' @export
#'
#' @examples
#' connect(db,host,user,pwd,port)
connect = function(db = Sys.getenv("db"), host = Sys.getenv('host'), user = Sys.getenv('user'), pwd = Sys.getenv('pwd'),port = as.integer(Sys.getenv("port"))){
  if (!app_require_packages(c("DBI", "RMySQL"), feature = "Database connectivity", notify = FALSE)) {
    message(format_missing_packages_message("Database connectivity", missing_required_packages(c("DBI", "RMySQL"))))
    return(NULL)
  }
  con = tryCatch({DBI::dbConnect(RMySQL::MySQL(),
                       host = host,
                       dbname = db,
                       user = user,
                       password = pwd,
                       port = port
  )
  }, error = function(e){
    message("Error connecting to database")
    message(e)
    return(NULL)
  })
  return(con)
}
