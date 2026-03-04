test_that("resolve_filters_below_plot_default handles missing and invalid width", {
  expect_null(ArchaeoDash:::resolve_filters_below_plot_default(numeric(0)))
  expect_null(ArchaeoDash:::resolve_filters_below_plot_default(NA_real_))
  expect_null(ArchaeoDash:::resolve_filters_below_plot_default("not-a-number"))
  expect_null(ArchaeoDash:::resolve_filters_below_plot_default(Inf))
})

test_that("resolve_filters_below_plot_default sets mobile/desktop defaults", {
  expect_true(ArchaeoDash:::resolve_filters_below_plot_default(320))
  expect_true(ArchaeoDash:::resolve_filters_below_plot_default("768"))
  expect_false(ArchaeoDash:::resolve_filters_below_plot_default(1024))
  expect_false(ArchaeoDash:::resolve_filters_below_plot_default("769"))
})
