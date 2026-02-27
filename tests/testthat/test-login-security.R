test_that("remember token generation and hashing behave as expected", {
  skip_if_not_installed("sodium")

  token <- make_remember_token()
  expect_true(is.character(token))
  expect_true(nzchar(token))
  expect_match(token, "^[0-9a-f]+$")

  hash1 <- hash_remember_token(token)
  hash2 <- hash_remember_token(token)
  expect_equal(hash1, hash2)
  expect_match(hash1, "^[0-9a-f]+$")
  expect_false(identical(hash1, hash_remember_token(paste0(token, "x"))))
})

test_that("login rate limit blocks after repeated failures and clears after success", {
  key <- paste0("test|", as.integer(Sys.time()))

  first <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_true(first$allowed)

  record_login_failure(key)
  record_login_failure(key)
  second <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_true(second$allowed)

  record_login_failure(key)
  blocked <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_false(blocked$allowed)
  expect_true(blocked$retry_after >= 1L)

  clear_login_failures(key)
  cleared <- check_login_rate_limit(key, max_attempts = 3L, window_seconds = 60L)
  expect_true(cleared$allowed)
})
