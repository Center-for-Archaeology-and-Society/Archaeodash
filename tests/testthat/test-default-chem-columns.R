test_that("default_chem_columns selects known INAA elements when present", {
  columns <- c("anid", "core", "Fe", "La", "custom_numeric")

  selected <- default_chem_columns(columns)

  expect_equal(selected, c("Fe", "La"))
})

test_that("default_chem_columns falls back to non-rowid/anid columns", {
  columns <- c("rowid", "anid", "sample", "custom_numeric")

  selected <- default_chem_columns(columns)

  expect_equal(selected, c("sample", "custom_numeric"))
})
