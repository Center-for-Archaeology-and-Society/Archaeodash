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
