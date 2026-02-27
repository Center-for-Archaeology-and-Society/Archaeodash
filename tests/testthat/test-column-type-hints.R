test_that("guess_numeric_columns_fast detects numeric-like columns without full coercion", {
  df <- tibble::tibble(
    rowid = as.character(seq_len(6)),
    grp = c("A", "B", "A", "B", "A", "B"),
    fe = c("1.0", "2.1", "3.2", "4.3", "5.4", "6.5"),
    mixed = c("1", "x", "3", "y", "5", "z"),
    num_native = c(1, 2, 3, 4, 5, 6)
  )

  out <- guess_numeric_columns_fast(df, exclude = "rowid", sample_n = 6, min_parse_rate = 0.8)
  expect_true("fe" %in% out)
  expect_true("num_native" %in% out)
  expect_false("grp" %in% out)
  expect_false("mixed" %in% out)
})

test_that("guess_numeric_columns_fast handles empty and invalid inputs safely", {
  expect_identical(guess_numeric_columns_fast(NULL), character())
  expect_identical(guess_numeric_columns_fast(tibble::tibble()), character())
  expect_identical(
    guess_numeric_columns_fast(tibble::tibble(rowid = "1", grp = "A"), exclude = c("rowid", "grp")),
    character()
  )
})
