test_that("updateCurrent writes combined workspace changes back to original source datasets", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbWriteTable(
    con,
    "demo_user_dataset_a",
    data.frame(rowid = "1", grp = "A", al = "1.0", stringsAsFactors = FALSE),
    row.names = FALSE
  )
  DBI::dbWriteTable(
    con,
    "demo_user_dataset_b",
    data.frame(rowid = "1", grp = "B", al = "2.0", stringsAsFactors = FALSE),
    row.names = FALSE
  )

  rvals <- shiny::reactiveValues(
    importedData = tibble::tibble(
      rowid = c("1", "2"),
      grp = c("A_new", "B_new"),
      al = c("1.0", "2.0")
    ),
    selectedData = tibble::tibble(
      rowid = c("1", "2"),
      grp = c("A_new", "B_new"),
      al = c("1.0", "2.0")
    ),
    chem = "al",
    currentDatasetName = c("demo_user_dataset_a", "demo_user_dataset_b"),
    currentDatasetRowMap = tibble::tibble(
      rowid = c("1", "2"),
      dataset_name = c("demo_user_dataset_a", "demo_user_dataset_b"),
      source_rowid = c("1", "1")
    )
  )
  credentials <- shiny::reactiveValues(
    status = TRUE,
    res = tibble::tibble(username = "demo_user")
  )

  expect_no_error(
    shiny::isolate(
      updateCurrent(
        rvals = rvals,
        con = con,
        credentials = credentials,
        input = list(selectedDatasets = c("demo_user_dataset_a", "demo_user_dataset_b")),
        output = NULL,
        session = NULL
      )
    )
  )

  out_a <- DBI::dbReadTable(con, "demo_user_dataset_a")
  out_b <- DBI::dbReadTable(con, "demo_user_dataset_b")
  expect_equal(as.character(out_a$grp), "A_new")
  expect_equal(as.character(out_b$grp), "B_new")
  expect_equal(as.character(out_a$rowid), "1")
  expect_equal(as.character(out_b$rowid), "1")
})
