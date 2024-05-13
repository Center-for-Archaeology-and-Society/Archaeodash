# connect

#@export
connect = function(db = Sys.getenv("db"), host = Sys.getenv('host'), user = Sys.getenv('user'), pwd = Sys.getenv('pwd'),port = as.integer(Sys.getenv("port"))){
  con = DBI::dbConnect(RMySQL::MySQL(),
                       host = host,
                       dbname = db,
                       user = user,
                       password = pwd,
                       port = port
  )
  return(con)
}
