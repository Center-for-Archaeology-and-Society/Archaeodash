test_that("db_table_exists_safe handles invalid input", {
  expect_false(db_table_exists_safe(NULL, "x"))
  expect_false(db_table_exists_safe(NULL, ""))
})

test_that("db_write_table_safe and db_remove_table_safe work with sqlite", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)

  expect_true(db_write_table_safe(con, "tmp_tbl", df, row.names = FALSE, context = "test write"))
  expect_true(db_table_exists_safe(con, "tmp_tbl"))

  loaded <- DBI::dbReadTable(con, "tmp_tbl")
  expect_equal(nrow(loaded), 2)

  expect_true(db_remove_table_safe(con, "tmp_tbl", context = "test remove"))
  expect_false(db_table_exists_safe(con, "tmp_tbl"))

  # removing an already-missing table should be a no-op success
  expect_true(db_remove_table_safe(con, "tmp_tbl", context = "test remove missing"))
})
