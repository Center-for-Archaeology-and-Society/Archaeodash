test_that("dataLoader replaces incoming rowid and preserves non-rowid columns as character", {
  tf <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      "A Name" = c("x", "y"),
      rowid = c(99, 100),
      check.names = FALSE
    ),
    tf,
    row.names = FALSE
  )

  data <- suppressWarnings(dataLoader(tf))

  expect_true("rowid" %in% names(data))
  expect_equal(data[["rowid"]], c(1L, 2L))
  expect_true("A_Name" %in% names(data))
  expect_equal(data[["A_Name"]], c("x", "y"))
  expect_identical(anyDuplicated(names(data)), 0L)
})

test_that("default_id_column prefers ANID case-insensitively", {
  cols <- c("rowid", "sample", "AnId", "group")
  expect_equal(default_id_column(cols), "AnId")
})

test_that("default_id_column returns empty when ANID is absent", {
  cols <- c("rowid", "sample", "group")
  expect_equal(default_id_column(cols), "")
})

test_that("resolve_id_column falls back to rowid when empty or invalid", {
  cols <- c("rowid", "anid", "sample")
  expect_equal(resolve_id_column("", cols), "rowid")
  expect_equal(resolve_id_column(NULL, cols), "rowid")
  expect_equal(resolve_id_column("missing", cols), "rowid")
  expect_equal(resolve_id_column("anid", cols), "anid")
})

test_that("replace_non_element_blanks fills empty and NA metadata fields only", {
  df <- tibble::tibble(
    rowid = c(1L, 2L, 3L),
    site = c("A", "", NA_character_),
    trench = c(" ", "T2", "T3"),
    Fe = c(1.1, NA_real_, 3.3)
  )

  out <- replace_non_element_blanks(df, chem_cols = c("Fe"))

  expect_equal(out$site, c("A", "[blank]", "[blank]"))
  expect_equal(out$trench, c("[blank]", "T2", "T3"))
  expect_true(is.numeric(out$Fe))
  expect_true(is.na(out$Fe[[2]]))
})

test_that("replace_non_element_blanks leaves data unchanged when no metadata columns", {
  df <- tibble::tibble(rowid = c(1L, 2L), Fe = c(1.0, 2.0))
  out <- replace_non_element_blanks(df, chem_cols = c("Fe"))
  expect_identical(out, df)
})

test_that("merge_loaded_data replaces or appends based on mode", {
  existing <- tibble::tibble(rowid = c("1", "2"), grp = c("A", "B"), Fe = c(1, 2))
  incoming <- tibble::tibble(rowid = c("1", "2"), grp = c("C", "D"), Fe = c(3, 4))

  replaced <- merge_loaded_data(existing, incoming, mode = "replace")
  expect_equal(nrow(replaced), 2)
  expect_equal(as.character(replaced$rowid), c("1", "2"))
  expect_equal(as.character(replaced$grp), c("C", "D"))

  added <- merge_loaded_data(existing, incoming, mode = "add")
  expect_equal(nrow(added), 4)
  expect_equal(as.character(added$rowid), c("1", "2", "3", "4"))
  expect_equal(as.character(added$grp), c("A", "B", "C", "D"))
})

test_that("merge_loaded_data appends when shared column types differ as numeric vs character", {
  existing <- tibble::tibble(rowid = c("1", "2"), Fe = c(10.5, 11.2), grp = c("A", "B"))
  incoming <- tibble::tibble(rowid = c("1", "2"), Fe = c("12.1", "13.0"), grp = c("C", "D"))

  out <- merge_loaded_data(existing, incoming, mode = "add")

  expect_equal(nrow(out), 4)
  expect_true(is.numeric(out$Fe))
  expect_equal(out$Fe, c(10.5, 11.2, 12.1, 13.0))
})

test_that("merge_loaded_data falls back to character when mixed values are not numeric-like", {
  existing <- tibble::tibble(rowid = c("1", "2"), Fe = c(1.1, 2.2), grp = c("A", "B"))
  incoming <- tibble::tibble(rowid = c("1", "2"), Fe = c("n/a", "3.3"), grp = c("C", "D"))

  out <- merge_loaded_data(existing, incoming, mode = "add")

  expect_equal(nrow(out), 4)
  expect_true(is.character(out$Fe))
  expect_equal(out$Fe, c("1.1", "2.2", "n/a", "3.3"))
})
