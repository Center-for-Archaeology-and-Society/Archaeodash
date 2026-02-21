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

test_that("confirm prior dataset load completes with persisted legacy transformation metadata", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  dataset_name <- "demo_user_dataset_a"
  dataset_key <- ArchaeoDash:::build_dataset_key(dataset_name)
  tx_index <- ArchaeoDash:::transform_index_table("demo_user")
  tx_name <- "legacy_tx"
  tx_prefix <- ArchaeoDash:::transform_prefix("demo_user", dataset_key, tx_name)

  DBI::dbWriteTable(
    con,
    dataset_name,
    data.frame(rowid = c("1", "2"), grp = c("A", "B"), al = c("1.1", "2.2"), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    paste0(dataset_name, "_metadata"),
    data.frame(field = "variable", value = "al", stringsAsFactors = FALSE),
    row.names = FALSE
  )

  DBI::dbWriteTable(
    con,
    tx_index,
    data.frame(
      dataset_key = dataset_key,
      transformation_name = tx_name,
      table_prefix = tx_prefix,
      created = as.character(Sys.time()),
      stringsAsFactors = FALSE
    ),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    paste0(tx_prefix, "_selected"),
    data.frame(rowid = c("1", "2"), grp = c("A", "B"), al = c(1.1, 2.2), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    paste0(tx_prefix, "_meta"),
    data.frame(
      field = c("created", "attrGroups", "chem", "runPCA", "runUMAP", "runLDA"),
      value = c(as.character(Sys.time()), "grp", "al", "FALSE", "FALSE", "FALSE"),
      stringsAsFactors = FALSE
    ),
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
      session$setInputs(selectedDatasets = dataset_name)
      expect_no_error(session$setInputs(confirmPrior = 1))
      session$flushReact()

      expect_equal(nrow(isolate(rvals$importedData)), 2)
      expect_equal(nrow(isolate(rvals$selectedData)), 2)
      expect_true(tx_name %in% names(isolate(rvals$transformations)))
    }
  )
})

test_that("confirm prior with multiple datasets combines rows and stores source mapping", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "demo_user_dataset_a",
    data.frame(rowid = c("1", "2"), grp = c("A", "A"), al = c("1.1", "1.2"), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    "demo_user_dataset_b",
    data.frame(rowid = c("1", "2"), grp = c("B", "B"), al = c("2.1", "2.2"), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    "demo_user_dataset_a_metadata",
    data.frame(field = "variable", value = "al", stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    "demo_user_dataset_b_metadata",
    data.frame(field = "variable", value = "al", stringsAsFactors = FALSE),
    row.names = FALSE
  )

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(),
    selectedData = tibble::tibble(),
    transformations = list(),
    activeTransformation = NULL,
    currentDatasetName = NULL,
    currentDatasetKey = NULL,
    currentDatasetRowMap = NULL,
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
      session$setInputs(selectedDatasets = c("demo_user_dataset_a", "demo_user_dataset_b"))
      expect_no_error(session$setInputs(confirmPrior = 1))
      session$flushReact()

      expect_equal(nrow(isolate(rvals$importedData)), 4)
      expect_equal(nrow(isolate(rvals$selectedData)), 4)
      expect_equal(length(isolate(rvals$currentDatasetName)), 2)

      row_map <- isolate(rvals$currentDatasetRowMap)
      expect_true(inherits(row_map, "data.frame"))
      expect_equal(nrow(row_map), 4)
      expect_setequal(unique(as.character(row_map$dataset_name)), c("demo_user_dataset_a", "demo_user_dataset_b"))
    }
  )
})

test_that("confirm prior with multiple datasets does not load persisted transformations", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ds_a <- "demo_user_dataset_a"
  ds_b <- "demo_user_dataset_b"
  DBI::dbWriteTable(con, ds_a, data.frame(rowid = c("1", "2"), grp = c("A", "A"), al = c("1.1", "1.2"), stringsAsFactors = FALSE), row.names = FALSE)
  DBI::dbWriteTable(con, ds_b, data.frame(rowid = c("1", "2"), grp = c("B", "B"), al = c("2.1", "2.2"), stringsAsFactors = FALSE), row.names = FALSE)
  DBI::dbWriteTable(con, paste0(ds_a, "_metadata"), data.frame(field = "variable", value = "al", stringsAsFactors = FALSE), row.names = FALSE)
  DBI::dbWriteTable(con, paste0(ds_b, "_metadata"), data.frame(field = "variable", value = "al", stringsAsFactors = FALSE), row.names = FALSE)

  combo_key <- ArchaeoDash:::build_dataset_key(c(ds_a, ds_b))
  tx_index <- ArchaeoDash:::transform_index_table("demo_user")
  tx_name <- "should_not_load_for_combo"
  tx_prefix <- ArchaeoDash:::transform_prefix("demo_user", combo_key, tx_name)

  DBI::dbWriteTable(
    con,
    tx_index,
    data.frame(
      dataset_key = combo_key,
      transformation_name = tx_name,
      table_prefix = tx_prefix,
      created = as.character(Sys.time()),
      stringsAsFactors = FALSE
    ),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    paste0(tx_prefix, "_selected"),
    data.frame(rowid = c("1", "2"), grp = c("A", "B"), al = c(1.1, 2.1), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    paste0(tx_prefix, "_meta"),
    data.frame(
      field = c("created", "attrGroups", "chem", "runPCA", "runUMAP", "runLDA"),
      value = c(as.character(Sys.time()), "grp", "al", "FALSE", "FALSE", "FALSE"),
      stringsAsFactors = FALSE
    ),
    row.names = FALSE
  )

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(),
    selectedData = tibble::tibble(),
    transformations = list(),
    activeTransformation = NULL,
    currentDatasetName = NULL,
    currentDatasetKey = NULL,
    currentDatasetRowMap = NULL,
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
      session$setInputs(selectedDatasets = c(ds_a, ds_b))
      expect_no_error(session$setInputs(confirmPrior = 1))
      session$flushReact()

      expect_equal(length(isolate(rvals$transformations)), 0)
      expect_null(isolate(rvals$activeTransformation))
    }
  )
})

test_that("confirm prior load times out instead of hanging on stalled workspace load", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("shiny")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  dataset_name <- "demo_user_dataset_timeout"
  DBI::dbWriteTable(
    con,
    dataset_name,
    data.frame(rowid = c("1", "2"), grp = c("A", "B"), al = c("1.1", "2.2"), stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    paste0(dataset_name, "_metadata"),
    data.frame(field = "variable", value = "al", stringsAsFactors = FALSE),
    row.names = FALSE
  )

  original_timeout <- get("dataset_load_timeout_seconds", envir = asNamespace("ArchaeoDash"))
  original_loader <- get("load_selected_datasets_workspace", envir = asNamespace("ArchaeoDash"))
  assignInNamespace("dataset_load_timeout_seconds", function() 1L, ns = "ArchaeoDash")
  assignInNamespace(
    "load_selected_datasets_workspace",
    function(con, selected_datasets) {
      i <- 0L
      while (TRUE) i <- i + 1L
    },
    ns = "ArchaeoDash"
  )
  on.exit(assignInNamespace("dataset_load_timeout_seconds", original_timeout, ns = "ArchaeoDash"), add = TRUE)
  on.exit(assignInNamespace("load_selected_datasets_workspace", original_loader, ns = "ArchaeoDash"), add = TRUE)

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(),
    selectedData = tibble::tibble(),
    transformations = list(),
    activeTransformation = NULL,
    currentDatasetName = NULL,
    currentDatasetKey = NULL,
    currentDatasetRowMap = NULL,
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
      session$setInputs(selectedDatasets = dataset_name)
      expect_no_error(session$setInputs(confirmPrior = 1))
      session$flushReact()

      expect_equal(nrow(isolate(rvals$importedData)), 0)
      expect_equal(nrow(isolate(rvals$selectedData)), 0)
      expect_null(isolate(rvals$currentDatasetName))
    }
  )
})
