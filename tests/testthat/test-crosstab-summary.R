test_that("build_crosstab_summary returns counts by two columns", {
  data <- data.frame(
    grp = c("A", "A", "B"),
    typ = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )

  result <- build_crosstab_summary(data, "grp", "typ", "count")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("grp", "typ", "count") %in% names(result)))
  expect_equal(sum(result$count), 3)
})

test_that("build_crosstab_summary computes numeric summary by first column", {
  data <- data.frame(
    grp = c("A", "A", "B"),
    val = c("1", "3", "5"),
    stringsAsFactors = FALSE
  )

  result <- build_crosstab_summary(data, "grp", "val", "mean")

  expect_true("result-val" %in% names(result))
  expect_equal(result[["result-val"]][result$grp == "A"], 2)
  expect_equal(result[["result-val"]][result$grp == "B"], 5)
})

test_that("build_crosstab_summary errors for non-numeric summary input column", {
  data <- data.frame(
    grp = c("A", "A", "B"),
    val = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  expect_error(
    build_crosstab_summary(data, "grp", "val", "median"),
    "cannot be converted to numeric values"
  )
})

test_that("build_crosstab_summary errors for unknown summary function", {
  data <- data.frame(
    grp = c("A", "B"),
    val = c("1", "2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    build_crosstab_summary(data, "grp", "val", "sum"),
    "Unsupported summary function selected"
  )
})
