source(testthat::test_path("..", "..", "R", "visualizeassignTab.R"), local = TRUE)

test_that("resolve_filters_below_plot_default handles missing and invalid width", {
  expect_null(resolve_filters_below_plot_default(numeric(0)))
  expect_null(resolve_filters_below_plot_default(NA_real_))
  expect_null(resolve_filters_below_plot_default("not-a-number"))
  expect_null(resolve_filters_below_plot_default(Inf))
})

test_that("resolve_filters_below_plot_default sets mobile/desktop defaults", {
  expect_true(resolve_filters_below_plot_default(320))
  expect_true(resolve_filters_below_plot_default("768"))
  expect_false(resolve_filters_below_plot_default(1024))
  expect_false(resolve_filters_below_plot_default("769"))
})
