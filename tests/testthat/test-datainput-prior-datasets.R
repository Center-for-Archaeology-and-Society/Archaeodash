test_that("prior dataset selector handles NA username without crashing", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "demo_user_dataset_a", data.frame(rowid = 1, x = "a"), row.names = FALSE)

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(),
    selectedData = tibble::tibble(),
    transformations = list(),
    activeTransformation = NULL,
    currentDatasetName = NULL,
    currentDatasetKey = NULL,
    tbls = character()
  )
  credentials <- shiny::reactiveValues(
    status = TRUE,
    res = tibble::tibble(username = NA_character_)
  )
  server_fun <- function(input, output, session) {
    ArchaeoDash::dataInputServer(
      input = input,
      output = output,
      session = session,
      rvals = rvals,
      con = con,
      credentials = credentials
    )
  }

  expect_no_error({
    shiny::testServer(
      server_fun,
      {
        session$flushReact()
        expect_identical(isolate(rvals$tbls), character())
        expect_no_error(output$priorDatasets)
      }
    )
  })
})

test_that("prior dataset selector falls back when remembered dataset is unavailable", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "demo_user_dataset_a", data.frame(rowid = 1, x = "a"), row.names = FALSE)
  DBI::dbWriteTable(con, "demo_user_dataset_b", data.frame(rowid = 2, x = "b"), row.names = FALSE)
  DBI::dbWriteTable(
    con,
    "demo_user_preferences",
    data.frame(field = "lastOpenedDataset", value = "demo_user_deleted_dataset", stringsAsFactors = FALSE),
    row.names = FALSE
  )

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(),
    selectedData = tibble::tibble(),
    transformations = list(),
    activeTransformation = NULL,
    currentDatasetName = NULL,
    currentDatasetKey = NULL,
    tbls = character()
  )
  credentials <- shiny::reactiveValues(
    status = TRUE,
    res = tibble::tibble(username = "demo_user")
  )
  server_fun <- function(input, output, session) {
    ArchaeoDash::dataInputServer(
      input = input,
      output = output,
      session = session,
      rvals = rvals,
      con = con,
      credentials = credentials
    )
  }

  shiny::testServer(
    server_fun,
    {
      session$flushReact()
      expect_true(length(isolate(rvals$tbls)) >= 1)
      ui <- output$priorDatasets
      html <- htmltools::renderTags(ui)$html
      expect_match(html, "demo_user_dataset_a|demo_user_dataset_b")
      expect_match(html, "selected")
    }
  )
})

test_that("prior dataset selector handles NA remembered dataset value", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(con, "demo_user_dataset_a", data.frame(rowid = 1, x = "a"), row.names = FALSE)
  DBI::dbWriteTable(
    con,
    "demo_user_preferences",
    data.frame(field = "lastOpenedDataset", value = NA_character_, stringsAsFactors = FALSE),
    row.names = FALSE
  )

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(),
    selectedData = tibble::tibble(),
    transformations = list(),
    activeTransformation = NULL,
    currentDatasetName = NA_character_,
    currentDatasetKey = NULL,
    tbls = character()
  )
  credentials <- shiny::reactiveValues(
    status = TRUE,
    res = tibble::tibble(username = "demo_user")
  )
  server_fun <- function(input, output, session) {
    ArchaeoDash::dataInputServer(
      input = input,
      output = output,
      session = session,
      rvals = rvals,
      con = con,
      credentials = credentials
    )
  }

  expect_no_error({
    shiny::testServer(
      server_fun,
      {
        session$flushReact()
        ui <- output$priorDatasets
        html <- htmltools::renderTags(ui)$html
        expect_match(html, "demo_user_dataset_a")
      }
    )
  })
})
