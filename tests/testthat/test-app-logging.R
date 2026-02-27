test_that("app logging helpers honor verbose option", {
  old_opt <- getOption("archaeodash.verbose")
  on.exit(options(archaeodash.verbose = old_opt), add = TRUE)

  options(archaeodash.verbose = FALSE)
  expect_false(app_is_verbose())
  expect_no_error(app_log("quiet log"))

  options(archaeodash.verbose = TRUE)
  expect_true(app_is_verbose())
  expect_no_error(app_log("verbose log"))
})
