if (!exists("should_refresh_dataset_tables", mode = "function")) {
  source(testthat::test_path("..", "..", "R", "datainputTab.R"), local = TRUE)
}
if (!exists("is_dataset_load_timeout_error", mode = "function")) {
  source(testthat::test_path("..", "..", "R", "datasetLoadTimeout.R"), local = TRUE)
}

test_that("is_dataset_load_timeout_error detects elapsed timeout errors", {
  err <- simpleError("reached elapsed time limit")
  expect_true(is_dataset_load_timeout_error(err))
  expect_false(is_dataset_load_timeout_error(simpleError("other database failure")))
  expect_false(is_dataset_load_timeout_error(NULL))
})

test_that("should_refresh_dataset_tables pauses refresh while loading", {
  expect_true(should_refresh_dataset_tables(FALSE, FALSE))
  expect_false(should_refresh_dataset_tables(TRUE, FALSE))
  expect_false(should_refresh_dataset_tables(FALSE, TRUE))
  expect_false(should_refresh_dataset_tables(TRUE, TRUE))
})

test_that("normalize_selected_datasets removes blanks and duplicates", {
  vals <- normalize_selected_datasets(c("a_tbl", "", "a_tbl", NA_character_, "b_tbl"))
  expect_identical(vals, c("a_tbl", "b_tbl"))
})

test_that("with_dataset_load_timeout returns expression result when under limit", {
  out <- with_dataset_load_timeout({ 1 + 1 }, timeout_sec = 1)
  expect_equal(out, 2)
})

test_that("with_dataset_load_timeout enforces timeout when limit exceeded", {
  expect_error(
    with_dataset_load_timeout({
      i <- 0L
      while (TRUE) i <- i + 1L
    }, timeout_sec = 1),
    "elapsed time limit"
  )
})
