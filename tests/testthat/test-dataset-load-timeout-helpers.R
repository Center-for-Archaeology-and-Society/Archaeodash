test_that("is_dataset_load_timeout_error detects elapsed timeout errors", {
  err <- simpleError("reached elapsed time limit")
  expect_true(is_dataset_load_timeout_error(err))
  expect_false(is_dataset_load_timeout_error(simpleError("other database failure")))
  expect_false(is_dataset_load_timeout_error(NULL))
})

test_that("with_dataset_load_timeout returns expression result when under limit", {
  out <- with_dataset_load_timeout({ 1 + 1 }, timeout_sec = 1)
  expect_equal(out, 2)
})

test_that("with_dataset_load_timeout enforces timeout when limit exceeded", {
  expect_error(
    with_dataset_load_timeout({
      i <- 0L
      while (TRUE) i <- i + 1L
    }, timeout_sec = 1),
    "elapsed time limit"
  )
})
