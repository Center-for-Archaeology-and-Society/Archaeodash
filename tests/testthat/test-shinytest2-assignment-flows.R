test_that("membership and euclidean checkbox assignment flows work end-to-end", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  testthat::skip_on_cran()
  chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) "")
  if (!is.character(chrome_path) || length(chrome_path) == 0 || !nzchar(chrome_path[[1]])) {
    testthat::skip("Chrome/Chromium not available for shinytest2 run.")
  }

  app_dir <- normalizePath(testthat::test_path("..", "..", "inst", "app"), mustWork = TRUE)
  csv_path <- normalizePath(testthat::test_path("..", "..", "inst", "app", "INAA_test.csv"), mustWork = TRUE)

  extract_dt_data <- function(widget_value) {
    if (!is.list(widget_value) || is.null(widget_value$x) || is.null(widget_value$x$data)) {
      return(tibble::tibble())
    }
    data <- widget_value$x$data
    if (is.data.frame(data)) return(tibble::as_tibble(data))
    as_tibble_try <- tryCatch(tibble::as_tibble(data), error = function(e) tibble::tibble())
    as_tibble_try
  }

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "assignment-flows",
    seed = 1
  )
  on.exit(app$stop(), add = TRUE)

  # Load data and create a confirmed transformation so assignment tabs are fully initialized.
  app$upload_file(file1 = csv_path)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_value(input = "loadcolumns", timeout = 20000)
  app$set_inputs(loadData = "click")
  app$wait_for_idle(timeout = 30000)
  app$wait_for_value(input = "attrGroups", timeout = 30000)
  app$set_inputs(action = "click")
  app$wait_for_idle(timeout = 10000)
  app$set_inputs(confirmTransformationAction = "click")
  app$wait_for_idle(timeout = 60000)

  # Membership: assign checked row to BestGroup, then to a new group.
  app$set_inputs(nav = "groupMembershiptab")
  app$wait_for_idle(timeout = 15000)
  app$set_inputs(membershipDataset = "elements")
  app$set_inputs(membershipRun = "click")
  app$wait_for_idle(timeout = 60000)

  membership_before <- extract_dt_data(app$get_value(output = "membershipTbl"))
  expect_true(nrow(membership_before) > 0)
  expect_true(all(c("rowid", "GroupVal", "BestGroup") %in% names(membership_before)))
  target_membership <- membership_before %>%
    dplyr::mutate(
      rowid = as.character(rowid),
      GroupVal = as.character(GroupVal),
      BestGroup = as.character(BestGroup)
    ) %>%
    dplyr::filter(!is.na(rowid), nzchar(rowid), !is.na(GroupVal), !is.na(BestGroup), GroupVal != BestGroup) %>%
    dplyr::slice_head(n = 1)
  if (nrow(target_membership) == 0) {
    target_membership <- membership_before %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::filter(!is.na(rowid), nzchar(rowid)) %>%
      dplyr::slice_head(n = 1)
  }
  expect_true(nrow(target_membership) == 1)
  target_rowid <- as.character(target_membership$rowid[[1]])

  app$set_inputs(membership_checked_rowids = target_rowid)
  app$set_inputs(gAssignBestGroup = "click")
  app$wait_for_idle(timeout = 30000)
  membership_after_best <- extract_dt_data(app$get_value(output = "membershipTbl"))
  updated_best <- membership_after_best %>%
    dplyr::mutate(rowid = as.character(rowid)) %>%
    dplyr::filter(rowid == target_rowid) %>%
    dplyr::slice_head(n = 1)
  expect_true(nrow(updated_best) == 1)
  expect_equal(as.character(updated_best$GroupVal[[1]]), as.character(updated_best$BestGroup[[1]]))

  new_group_membership <- "ZZ_TEST_MEMBERSHIP"
  app$set_inputs(membership_checked_rowids = target_rowid)
  app$set_inputs(gGroupAssignChoice = "__new__", gGroupAssignNew = new_group_membership)
  app$set_inputs(gChangeGroup = "click")
  app$wait_for_idle(timeout = 30000)
  membership_after_new <- extract_dt_data(app$get_value(output = "membershipTbl"))
  updated_new <- membership_after_new %>%
    dplyr::mutate(rowid = as.character(rowid), GroupVal = as.character(GroupVal)) %>%
    dplyr::filter(rowid == target_rowid) %>%
    dplyr::slice_head(n = 1)
  expect_true(nrow(updated_new) == 1)
  expect_equal(updated_new$GroupVal[[1]], new_group_membership)

  # Euclidean Distance: assign checked row to match group, then to a new group.
  app$set_inputs(nav = "euclideanDistancetab")
  app$wait_for_idle(timeout = 15000)
  app$set_inputs(EDdataset = "elements")
  app$set_inputs(EDRun = "click")
  app$wait_for_idle(timeout = 60000)

  ed_before <- extract_dt_data(app$get_value(output = "EDTbl"))
  expect_true(nrow(ed_before) > 0)
  expect_true("rowid" %in% names(ed_before))
  match_col <- grep("_match$", names(ed_before), value = TRUE)[1]
  expect_true(!is.na(match_col))
  group_col <- sub("_match$", "", match_col)
  expect_true(group_col %in% names(ed_before))

  target_ed <- ed_before %>%
    dplyr::mutate(
      rowid = as.character(rowid),
      group_val = as.character(.data[[group_col]]),
      match_val = as.character(.data[[match_col]])
    ) %>%
    dplyr::filter(!is.na(rowid), nzchar(rowid), !is.na(group_val), !is.na(match_val), group_val != match_val) %>%
    dplyr::slice_head(n = 1)
  if (nrow(target_ed) == 0) {
    target_ed <- ed_before %>%
      dplyr::mutate(rowid = as.character(rowid)) %>%
      dplyr::filter(!is.na(rowid), nzchar(rowid)) %>%
      dplyr::slice_head(n = 1)
  }
  expect_true(nrow(target_ed) == 1)
  target_ed_rowid <- as.character(target_ed$rowid[[1]])

  app$set_inputs(ed_checked_rowids = target_ed_rowid)
  app$set_inputs(edAssignMatchGroup = "click")
  app$wait_for_idle(timeout = 30000)
  ed_after_match <- extract_dt_data(app$get_value(output = "EDTbl"))
  updated_match <- ed_after_match %>%
    dplyr::mutate(rowid = as.character(rowid)) %>%
    dplyr::filter(rowid == target_ed_rowid) %>%
    dplyr::slice_head(n = 1)
  expect_true(nrow(updated_match) == 1)
  expect_equal(as.character(updated_match[[group_col]][[1]]), as.character(updated_match[[match_col]][[1]]))

  new_group_ed <- "ZZ_TEST_ED"
  app$set_inputs(ed_checked_rowids = target_ed_rowid)
  app$set_inputs(edGroupAssignChoice = "__new__", edGroupAssignNew = new_group_ed)
  app$set_inputs(edChangeGroup = "click")
  app$wait_for_idle(timeout = 30000)
  ed_after_new <- extract_dt_data(app$get_value(output = "EDTbl"))
  updated_ed_new <- ed_after_new %>%
    dplyr::mutate(rowid = as.character(rowid), group_val = as.character(.data[[group_col]])) %>%
    dplyr::filter(rowid == target_ed_rowid) %>%
    dplyr::slice_head(n = 1)
  expect_true(nrow(updated_ed_new) == 1)
  expect_equal(updated_ed_new$group_val[[1]], new_group_ed)
})
