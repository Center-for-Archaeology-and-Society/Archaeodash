source(testthat::test_path("..", "..", "R", "datasetNameHelpers.R"), local = TRUE)

test_that("build_dataset_table_name keeps short names unchanged", {
  out <- build_dataset_table_name("user", "my_dataset", max_len = app_table_name_max_len)
  expect_identical(out, "user_my_dataset")
  expect_lte(nchar(out), app_table_name_max_len)
})

test_that("build_dataset_table_name shortens long names deterministically", {
  long_label <- paste(rep("verylongdatasetname", 6), collapse = "_")
  out1 <- build_dataset_table_name("long_user_name", long_label, max_len = app_table_name_max_len)
  out2 <- build_dataset_table_name("long_user_name", long_label, max_len = app_table_name_max_len)
  expect_identical(out1, out2)
  expect_lte(nchar(out1), app_table_name_max_len)
  expect_true(grepl("_[0-9a-f]{8}$", out1))
})

test_that("build_user_preferences_table_name respects policy max length", {
  out <- build_user_preferences_table_name("very_long_username_for_preference_tables")
  expect_lte(nchar(out), app_table_name_max_len)
  expect_match(out, "_preferences$")
})
