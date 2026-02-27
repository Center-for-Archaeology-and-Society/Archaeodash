test_that("calculate_mahalanobis_distance returns expected scalar output", {
  x_bar <- c(1, 2)
  y_bar <- c(2, 3)
  f0 <- 1
  Sx <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  Sy <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  m <- 10

  distance <- calculate_mahalanobis_distance(x_bar, y_bar, f0, Sx, Sy, m)

  expect_type(distance, "double")
  expect_equal(distance, 0.07936508, tolerance = 1e-8)
})
