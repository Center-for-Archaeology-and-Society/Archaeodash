library(plumber)

# Create the main Plumber router
pr <- pr()

# Add a default root endpoint at /
#* @get /
#* @serializer json
function(req, res) {
  list(
    status = "ok",
    message = "Archaeodash API is running",
    time = as.character(Sys.time())
  )
}

# Mount the auth endpoints
pr$mount("/", pr("api-auth.R"))

if (interactive()) {
  pr$run(host = "0.0.0.0", port = 5000)
}



pr
