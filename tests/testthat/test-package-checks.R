test_that("missing_required_packages returns only unavailable packages", {
  missing <- missing_required_packages(c("stats", "definitelyNotARealPackage"))
  expect_true("definitelyNotARealPackage" %in% missing)
  expect_false("stats" %in% missing)
})

test_that("format_missing_packages_message includes feature and package list", {
  msg <- format_missing_packages_message("UMAP", c("umap", "plotly"))
  expect_match(msg, "UMAP")
  expect_match(msg, "umap, plotly")
})

test_that("app_require_packages returns TRUE when all packages are present", {
  expect_true(app_require_packages("stats", feature = "Test feature", notify = FALSE))
})

test_that("app_require_packages returns FALSE when package is missing", {
  expect_false(app_require_packages("definitelyNotARealPackage", feature = "Test feature", notify = FALSE))
})
