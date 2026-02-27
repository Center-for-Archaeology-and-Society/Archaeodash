test_that("available_group_assignments returns sorted non-empty groups", {
  df <- tibble::tibble(
    grp = c("B", "A", "", NA, "A"),
    x = 1:5
  )
  expect_equal(available_group_assignments(df, "grp"), c("A", "B"))
  expect_equal(available_group_assignments(df, "missing"), character())
  expect_equal(available_group_assignments(df, NA_character_), character())
})

test_that("resolve_group_assignment_target handles existing and new group modes", {
  expect_equal(resolve_group_assignment_target("Alpha", "ignored"), "Alpha")
  expect_equal(resolve_group_assignment_target("__new__", "  NewGroup  "), "NewGroup")
  expect_equal(resolve_group_assignment_target("__new__", NA_character_), "")
  expect_equal(resolve_group_assignment_target(NA_character_, "ignored"), "")
})

test_that("build_group_assignment_ui includes select and conditional new group input", {
  ui <- build_group_assignment_ui(
    choice_input_id = "choiceId",
    new_input_id = "newId",
    groups = c("G2", "G1"),
    selected_choice = "G1"
  )
  html <- htmltools::renderTags(ui)$html
  expect_match(html, "choiceId")
  expect_match(html, "newId")
  expect_match(html, "Create new group")
})

test_that("build_group_assignment_ui handles NA selected_choice without error", {
  expect_no_error({
    ui <- build_group_assignment_ui(
      choice_input_id = "choiceIdNA",
      new_input_id = "newIdNA",
      groups = c("G1", "G2"),
      selected_choice = NA_character_
    )
    html <- htmltools::renderTags(ui)$html
    expect_match(html, "choiceIdNA")
    expect_match(html, "G1")
  })
})

test_that("add_checkbox_column prepends checkbox column and preserves checked state", {
  df <- tibble::tibble(rowid = c("1", "2"), val = c("a", "b"))
  out <- add_checkbox_column(
    df = df,
    checked_rowids = c("2"),
    rowid_col = "rowid",
    checkbox_col = ".select",
    checkbox_class = "x-check"
  )
  expect_true(names(out)[1] == ".select")
  expect_match(out$.select[[1]], "data-rowid=\"1\"")
  expect_false(grepl("checked", out$.select[[1]], fixed = TRUE))
  expect_match(out$.select[[2]], "data-rowid=\"2\"")
  expect_true(grepl("checked", out$.select[[2]], fixed = TRUE))
})
