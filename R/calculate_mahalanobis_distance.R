#' Calculate Squared Mahalanobis Distance
#'
#' This function calculates the squared Mahalanobis distance between two mean vectors,
#' given the scaling factor, covariance matrices, and sample size.
#'
#' @param x_bar A numeric vector representing the mean vector \eqn{\bar{x}}.
#' @param y_bar A numeric vector representing the mean vector \eqn{\bar{y}}.
#' @param f0 A numeric value representing the scaling factor \eqn{f_0}.
#' @param Sx A numeric matrix representing the covariance matrix \eqn{S_x}.
#' @param Sy A numeric matrix representing the covariance matrix \eqn{S_y}.
#' @param m An integer representing the sample size \eqn{m}.
#'
#' @return A numeric value representing the squared Mahalanobis distance.
#' @export
#'
#' @examples
#' x_bar <- c(1, 2)
#' y_bar <- c(2, 3)
#' f0 <- 1
#' Sx <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
#' Sy <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
#' m <- 10
#'
#' distance <- calculate_mahalanobis_distance(x_bar, y_bar, f0, Sx, Sy, m)
#' print(distance)
calculate_mahalanobis_distance <- function(x_bar, y_bar, f0, Sx, Sy, m) {
  # Ensure the inputs are in matrix form
  x_bar <- as.matrix(x_bar)
  y_bar <- as.matrix(y_bar)
  Sx <- as.matrix(Sx)
  Sy <- as.matrix(Sy)

  # Calculate the difference vector
  diff_vector <- f0 * x_bar - y_bar

  # Calculate the combined covariance matrix
  combined_covariance <- f0^2 * Sx + Sy

  # Calculate the inverse of the combined covariance matrix
  combined_covariance_inv <- solve(combined_covariance)

  # Calculate the squared Mahalanobis distance
  distance_squared <- (1 / (m - 1)) * t(diff_vector) %*% combined_covariance_inv %*% diff_vector

  # Return the distance as a scalar
  return(as.numeric(distance_squared))
}
