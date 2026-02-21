test_that("write_user_preference_safe creates preference table and persists value", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ok <- ArchaeoDash:::write_user_preference_safe(
    con = con,
    username = "demo_user",
    field = "lastOpenedDataset",
    value = "demo_user_dataset_a"
  )
  expect_true(isTRUE(ok))

  prefs <- ArchaeoDash:::read_user_preferences_safe(con = con, username = "demo_user")
  expect_true(inherits(prefs, "data.frame"))
  expect_equal(nrow(prefs), 1)
  expect_equal(as.character(prefs$field[[1]]), "lastOpenedDataset")
  expect_equal(as.character(prefs$value[[1]]), "demo_user_dataset_a")
})

test_that("write_user_preference_safe updates existing fields without duplicate insert", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  pref_tbl <- ArchaeoDash:::build_user_preferences_table_name("demo_user", max_len = ArchaeoDash:::app_table_name_max_len)
  DBI::dbWriteTable(
    con,
    pref_tbl,
    data.frame(
      field = c("lastOpenedDataset", "themePreference"),
      value = c("demo_user_old_dataset", "light"),
      stringsAsFactors = FALSE
    ),
    row.names = FALSE
  )

  ok <- ArchaeoDash:::write_user_preference_safe(
    con = con,
    username = "demo_user",
    field = "lastOpenedDataset",
    value = "demo_user_new_dataset"
  )
  expect_true(isTRUE(ok))

  prefs <- ArchaeoDash:::read_user_preferences_safe(con = con, username = "demo_user")
  expect_equal(sum(as.character(prefs$field) == "lastOpenedDataset"), 1)
  expect_equal(
    ArchaeoDash:::get_user_preference_safe(con = con, username = "demo_user", field = "lastOpenedDataset", default_value = ""),
    "demo_user_new_dataset"
  )
  expect_equal(
    ArchaeoDash:::get_user_preference_safe(con = con, username = "demo_user", field = "themePreference", default_value = ""),
    "light"
  )
})
