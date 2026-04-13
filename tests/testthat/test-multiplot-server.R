source(testthat::test_path("..", "..", "R", "visualizeassignTab.R"), local = TRUE)
source(testthat::test_path("..", "..", "R", "plot.R"), local = TRUE)
source(testthat::test_path("..", "..", "R", "quietly.R"), local = TRUE)

if (!exists("app_log", mode = "function")) {
  app_log <- function(...) invisible(NULL)
}
if (!exists("app_is_verbose", mode = "function")) {
  app_is_verbose <- function() FALSE
}
if (!exists("mynotification", mode = "function")) {
  mynotification <- function(...) invisible(NULL)
}
if (!exists("replaceCell", mode = "function")) {
  replaceCell <- function(...) invisible(NULL)
}

make_multiplot_rvals <- function() {
  set.seed(42)
  selected_data <- data.frame(
    rowid = as.character(seq_len(12)),
    grp = rep(c("A", "B"), each = 6),
    V1 = rnorm(12),
    V2 = rnorm(12),
    V3 = rnorm(12),
    stringsAsFactors = FALSE
  )
  shiny::reactiveValues(
    selectedData = selected_data,
    attrGroups = "grp",
    chem = c("V1", "V2", "V3"),
    multiplot = NULL,
    plotdf = selected_data,
    brushSelected = NULL,
    sampleID = NULL
  )
}

test_that("multiplot sync path builds ggplot and clears loader on success", {
  old_force_sync <- getOption("archaeodash.multiplot.force_sync")
  options(archaeodash.multiplot.force_sync = TRUE)
  on.exit(options(archaeodash.multiplot.force_sync = old_force_sync), add = TRUE)

  rvals <- make_multiplot_rvals()
  server_fun <- function(input, output, session) {
    visualizeAssignServer(
      input = input, output = output, session = session,
      rvals = rvals,
      credentials = shiny::reactiveValues(status = FALSE, res = data.frame(username = NA_character_)),
      con = NULL
    )
  }

  shiny::testServer(server_fun, {
    session$setInputs(
      xvar2    = "V1",
      yvar2    = "V2",
      interactive = "FALSE",
      ptsize   = 2,
      plot_theme  = "viridis",
      plotHeight  = 900
    )
    session$setInputs(updateMultiplot = 1)
    session$flushReact()
    expect_s3_class(rvals$multiplot, "ggplot")
  })
})

test_that("multiplot sync path builds plotly when interactive=TRUE", {
  old_force_sync <- getOption("archaeodash.multiplot.force_sync")
  options(archaeodash.multiplot.force_sync = TRUE)
  on.exit(options(archaeodash.multiplot.force_sync = old_force_sync), add = TRUE)

  rvals <- make_multiplot_rvals()
  server_fun <- function(input, output, session) {
    visualizeAssignServer(
      input = input, output = output, session = session,
      rvals = rvals,
      credentials = shiny::reactiveValues(status = FALSE, res = data.frame(username = NA_character_)),
      con = NULL
    )
  }

  shiny::testServer(server_fun, {
    session$setInputs(
      xvar2    = "V1",
      yvar2    = "V2",
      interactive = "TRUE",
      ptsize   = 2,
      plot_theme  = "viridis",
      plotHeight  = 900
    )
    session$setInputs(updateMultiplot = 1)
    session$flushReact()
    expect_s3_class(rvals$multiplot, "plotly")
  })
})

test_that("multiplot sync path leaves rvals$multiplot NULL when no data", {
  old_force_sync <- getOption("archaeodash.multiplot.force_sync")
  options(archaeodash.multiplot.force_sync = TRUE)
  on.exit(options(archaeodash.multiplot.force_sync = old_force_sync), add = TRUE)

  rvals <- make_multiplot_rvals()
  rvals$selectedData <- data.frame()
  server_fun <- function(input, output, session) {
    visualizeAssignServer(
      input = input, output = output, session = session,
      rvals = rvals,
      credentials = shiny::reactiveValues(status = FALSE, res = data.frame(username = NA_character_)),
      con = NULL
    )
  }

  shiny::testServer(server_fun, {
    session$setInputs(
      xvar2    = "V1",
      yvar2    = "V2",
      interactive = "FALSE",
      ptsize   = 2,
      plot_theme  = "viridis",
      plotHeight  = 900
    )
    session$setInputs(updateMultiplot = 1)
    session$flushReact()
    expect_null(rvals$multiplot)
  })
})

test_that("multiplot sync path leaves rvals$multiplot NULL when axes overlap", {
  old_force_sync <- getOption("archaeodash.multiplot.force_sync")
  options(archaeodash.multiplot.force_sync = TRUE)
  on.exit(options(archaeodash.multiplot.force_sync = old_force_sync), add = TRUE)

  rvals <- make_multiplot_rvals()
  server_fun <- function(input, output, session) {
    visualizeAssignServer(
      input = input, output = output, session = session,
      rvals = rvals,
      credentials = shiny::reactiveValues(status = FALSE, res = data.frame(username = NA_character_)),
      con = NULL
    )
  }

  shiny::testServer(server_fun, {
    session$setInputs(
      xvar2    = "V1",
      yvar2    = "V1",
      interactive = "FALSE",
      ptsize   = 2,
      plot_theme  = "viridis",
      plotHeight  = 900
    )
    session$setInputs(updateMultiplot = 1)
    session$flushReact()
    expect_null(rvals$multiplot)
  })
})
