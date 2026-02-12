test_that("zScore returns expected rounded row-wise percentage z-scores", {
  x <- data.frame(a = c(1, 1), b = c(1, 3))

  result <- zScore(x)

  expect_s3_class(result, "data.frame")
  expect_equal(
    result,
    data.frame(
      a = c(0.707, -0.707),
      b = c(-0.707, 0.707)
    )
  )
})
