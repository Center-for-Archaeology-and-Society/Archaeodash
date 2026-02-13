test_that("resolve_group_selection defaults to all groups when switching columns", {
  all_groups <- c("A", "B", "C")
  prior_selection <- c("A")

  selected <- resolve_group_selection(
    all_groups = all_groups,
    prior_selection = prior_selection,
    prior_group_column = "old_group",
    current_group_column = "new_group"
  )

  expect_equal(selected, all_groups)
})

test_that("resolve_group_selection preserves valid prior selection on same column", {
  selected <- resolve_group_selection(
    all_groups = c("A", "B", "C"),
    prior_selection = c("B", "Z"),
    prior_group_column = "group_col",
    current_group_column = "group_col"
  )

  expect_equal(selected, "B")
})

test_that("resolve_group_selection preserves explicit empty selection on same column", {
  selected <- resolve_group_selection(
    all_groups = c("A", "B", "C"),
    prior_selection = character(),
    prior_group_column = "group_col",
    current_group_column = "group_col"
  )

  expect_equal(selected, character())
})

test_that("filter_group_choices returns expected subsets by mode", {
  all_groups <- c("A", "B", "C")
  selection <- c("A", "C")

  expect_equal(
    filter_group_choices(all_groups, selection, mode = "all"),
    all_groups
  )
  expect_equal(
    filter_group_choices(all_groups, selection, mode = "selected"),
    c("A", "C")
  )
  expect_equal(
    filter_group_choices(all_groups, selection, mode = "unselected"),
    "B"
  )
})

test_that("merge_group_selection handles selected and unselected modes correctly", {
  all_groups <- c("A", "B", "C")

  selected_mode <- merge_group_selection(
    prior_selection = c("A", "B"),
    picked_values = c("B"),
    all_groups = all_groups,
    mode = "selected"
  )
  expect_equal(selected_mode, "B")

  unselected_mode <- merge_group_selection(
    prior_selection = c("A"),
    picked_values = c("B"),
    all_groups = all_groups,
    mode = "unselected"
  )
  expect_equal(unselected_mode, c("A", "B"))
})

test_that("filter_group_choices handles empty selections and groups", {
  expect_equal(filter_group_choices(character(), character(), mode = "all"), character())
  expect_equal(filter_group_choices(c("A", "B"), character(), mode = "selected"), character())
  expect_equal(filter_group_choices(c("A", "B"), c("A"), mode = "unselected"), "B")
})
