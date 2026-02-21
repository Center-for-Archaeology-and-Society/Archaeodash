#' server.R
library(ArchaeoDash)
library(shiny)
library(profvis)

# options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 100 * 1024^2)

shinyServer(function(input, output, session) {
  ArchaeoDash:::ensure_shiny_dependency_aliases()

  ###  create reactive values  ####
  rvals = reactiveValues(importedData = tibble::tibble(),
  selectedData = tibble::tibble())
  rvals$transformations <- list()
  rvals$activeTransformation <- NULL
  rvals$currentDatasetName <- NULL
  rvals$currentDatasetKey <- NULL
  credentials = reactiveValues()
  # for testing
  # rvals <<- reactiveValues(importedData = tibble::tibble(),
  # selectedData = tibble::tibble()); showNotification("warning: global variable is only for testing", type = "warning")
  # input <<- input; showNotification("warning: global variable is only for testing", type = "warning")
  # session <<- session; showNotification("warning: global variable is only for testing", type = "warning")
  # credentials <<- reactiveValues(); showNotification("warning: global variable is only for testing", type = "warning")

  db_connect_error <- NULL
  con = tryCatch(connect(), error = function(e) {
    db_connect_error <<- conditionMessage(e)
    NULL
  })

  if (is.null(con)) {
    observeEvent(TRUE, {
      showNotification(
        paste0(
          "Unable to connect to database. Some features will be disabled.",
          if (!is.null(db_connect_error) && nzchar(db_connect_error)) paste0(" Details: ", db_connect_error) else ""
        ),
        type = "error",
        duration = NULL
      )
    }, once = TRUE, ignoreInit = FALSE)
  } else if (identical(Sys.getenv("ARCHAEODASH_RUN_TABLE_MIGRATION", "0"), "1")) {
    try(ArchaeoDash:::migrate_table_names_to_32(con), silent = TRUE)
  }
  session$onSessionEnded(function() {
    if (!is.null(con)) {
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  })

  loginUI(input = input)

  loginServer(con, input = input, output = output, session = session, credentials = credentials)

  safe_username <- function() {
    raw_username <- tryCatch(credentials$res$username[[1]], error = function(e) "")
    if (is.null(raw_username) || length(raw_username) == 0 || is.na(raw_username)) return("")
    as.character(raw_username)
  }

  read_user_preferences <- function(username) {
    ArchaeoDash:::read_user_preferences_safe(
      con = con,
      username = username,
      max_len = ArchaeoDash:::app_table_name_max_len
    )
  }

  write_user_preference <- function(username, field, value) {
    if (is.null(con) || !nzchar(username) || !nzchar(field) || !nzchar(value)) return(invisible(NULL))
    try(
      ArchaeoDash:::write_user_preference_safe(
        con = con,
        username = username,
        field = field,
        value = as.character(value),
        max_len = ArchaeoDash:::app_table_name_max_len
      ),
      silent = TRUE
    )
    invisible(NULL)
  }

  get_saved_theme <- function(username) {
    if (!nzchar(username)) return("")
    prefs <- read_user_preferences(username)
    if (nrow(prefs) == 0) return("")
    theme <- prefs %>%
      dplyr::filter(.data$field == "themePreference") %>%
      dplyr::pull(.data$value)
    if (length(theme) == 0 || is.na(theme[[1]])) return("")
    candidate <- as.character(theme[[1]])
    if (!(candidate %in% c("simple", "light", "dark"))) return("")
    candidate
  }

  observeEvent(credentials$res$username, {
    if(!is.null(credentials$res$username) && !is.na(credentials$res$username)){
      output$userMessage = renderUI({
        renderText(paste("Logged in as", credentials$res$username))
      })
      shinyjs::hide(id = "loginUI")
      shinyjs::show(id = "logoutUI")
      saved_theme <- get_saved_theme(safe_username())
      if (nzchar(saved_theme)) {
        session$sendCustomMessage("theme_preference", list(theme = saved_theme))
      }
    } else {
      output$userMessage = renderUI({
        NULL
      })
      shinyjs::show(id = "loginUI")
      shinyjs::hide(id = "logoutUI")
    }
  })

  observeEvent(input$logoutUI,{
    credentials$res = tibble::tibble(username = NA)
    credentials$status = FALSE
    session$sendCustomMessage("auth_cookie", list(action = "clear"))
  })

  observeEvent(input$open_privacy_policy, {
    updateNavbarPage(session, "nav", selected = "privacy")
  })

  observeEvent(input$theme_preference_set, {
    if (!isTruthy(credentials$status)) return(NULL)
    selected_theme <- as.character(input$theme_preference_set[[1]])
    if (!(selected_theme %in% c("simple", "light", "dark"))) return(NULL)
    username <- safe_username()
    if (!nzchar(username)) return(NULL)
    write_user_preference(username, "themePreference", selected_theme)
  }, ignoreInit = TRUE)

  #### Import data ####

  quietly(label = "dataInputServer",dataInputServer(input,output,session,rvals,con, credentials))

  ####  Explore ####
  quietly(label = "exploreServer",exploreServer(input,output,session,rvals,con,credentials))

  ####   Ordination   ####
  quietly(label = "ordinationServer",ordinationServer(input,output,session,rvals))

  ####   Cluster  ####
  quietly(label = "clusterServer",clusterServer(input,output,session,rvals, credentials, con))

  ####   Group Membership  ####
  quietly(label = "groupServer",groupServer(input,output,session,rvals, credentials, con))

  ####   Euclidean Distance  ####
  quietly(label = "euclideanDistanceSrvr",euclideanDistanceSrvr(input,output,session,rvals, credentials, con))

  ####   Visualize & Assign  ####
  quietly(label = "visualizeAssignServer",visualizeAssignServer(input,output,session,rvals, credentials, con))

  ####   Save & Export  ####
  quietly(label = "saveExportServer",saveExportServer(input,output,session,rvals))

  #### subsetDataServer ####
  subsetDataServer(input,output,session,rvals) # no longer used?

}) # end server
