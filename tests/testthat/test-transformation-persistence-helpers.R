test_that("collapse and split helpers round trip values", {
  vals <- c("A", "B", "C")
  encoded <- collapse_transform_values(vals)
  decoded <- split_transform_values(encoded)
  expect_equal(decoded, vals)
})

test_that("safe table naming and keys are deterministic", {
  key1 <- build_dataset_key(c("user_b", "user_a"))
  key2 <- build_dataset_key(c("user_a", "user_b"))
  expect_equal(key1, key2)
  expect_true(nchar(key1) <= 40)

  tbl <- safe_table_name("User Name--Very Long Table Name!", max_len = 20)
  expect_true(nchar(tbl) <= 20)
  expect_true(grepl("^[a-z0-9_]+$", tbl))
})

test_that("transformation prefix is bounded length", {
  prefix <- transform_prefix(
    username = "very_long_username_for_testing",
    dataset_key = "a_really_long_dataset_key_that_should_be_trimmed",
    transformation_name = "a_really_long_transformation_name_that_should_be_trimmed"
  )
  expect_true(nchar(prefix) <= 55)
  expect_true(grepl("^[a-z0-9_]+$", prefix))
})
