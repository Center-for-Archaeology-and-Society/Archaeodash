source(testthat::test_path("..", "..", "R", "datasetNameHelpers.R"), local = TRUE)

test_that("build_dataset_table_name keeps short names unchanged", {
  out <- build_dataset_table_name("user", "my_dataset", max_len = 54)
  expect_identical(out, "user_my_dataset")
  expect_lte(nchar(out), 54)
})

test_that("build_dataset_table_name shortens long names deterministically", {
  long_label <- paste(rep("verylongdatasetname", 6), collapse = "_")
  out1 <- build_dataset_table_name("long_user_name", long_label, max_len = 54)
  out2 <- build_dataset_table_name("long_user_name", long_label, max_len = 54)
  expect_identical(out1, out2)
  expect_lte(nchar(out1), 54)
  expect_true(grepl("_[0-9a-f]{8}$", out1))
})
