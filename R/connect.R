
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
    app_log(format_missing_packages_message("Database connectivity", missing_required_packages(c("DBI", "RMySQL"))))
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
    app_log("Error connecting to database")
    app_log(conditionMessage(e))
    return(NULL)
  })
  if (!is.null(con)) {
    # Bound lock waits at the session level so blocked DML/metadata operations fail fast
    # instead of freezing Shiny observers for minutes/hours.
    try(DBI::dbExecute(con, "SET SESSION lock_wait_timeout = 5"), silent = TRUE)
    try(DBI::dbExecute(con, "SET SESSION innodb_lock_wait_timeout = 10"), silent = TRUE)
  }
  return(con)
}
