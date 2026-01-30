#' Pairwise Root Mean Squared Error (RMSE) with Row-Specific Masking
#'
#' Computes a pairwise RMSE-style distance matrix between all rows of a matrix.
#' For each pair of rows \eqn{(i, j)}, the RMSE is calculated only across
#' the columns where row \eqn{j} has nonzero entries. This follows the
#' GAUSS-style masking logic: the denominator is the number of nonzero
#' entries in row \eqn{j}, and rows with all zeros yield \code{NA} in
#' their corresponding column of the result.
#'
#' @param A A numeric matrix, where rows are observations and columns are variables.
#'          Zeros in a row indicate that those variables should be ignored when
#'          comparing to other rows.
#'
#' @return A numeric matrix of size \eqn{n \times n}, where \eqn{n} is the number of
#'         rows in \code{A}. Entry \eqn{D[i, j]} is the RMSE between row \eqn{i} and
#'         row \eqn{j}, computed over the nonzero positions of row \eqn{j}.
#'         The matrix is generally asymmetric because the mask depends on the
#'         reference row \eqn{j}.
#'
#' @details
#' The calculation for each pair \eqn{(i, j)} is:
#' \deqn{
#'   D[i, j] = \sqrt{ \frac{1}{p_j} \sum_{v: A[j, v] \neq 0} (A[i, v] - A[j, v])^2 }
#' }
#' where \eqn{p_j} is the number of nonzero entries in row \eqn{j}.
#'
#' @examples
#' mat <- matrix(c(1, 2, 0,
#'                 2, 2, 3,
#'                 0, 4, 5), nrow = 3, byrow = TRUE)
#' rownames(mat) <- paste0("row", 1:3)
#'
#' rmse_pairwise(mat)
#'
#' @export

rmse_pairwise <- function(A) {
  A <- as.matrix(A)
  n <- nrow(A)

  # Mask W[j, v] = 1 if A[j, v] != 0 else 0
  W <- (A != 0) * 1.0
  pvars <- rowSums(W)
  pvars[pvars == 0] <- NA_real_  # No valid vars for that row ⇒ column becomes NA

  A2 <- A^2

  # Sum_v W[j,v] * A[i,v]^2  -> (n x p) %*% (p x n) = (n x n)
  T1 <- A2 %*% t(W)

  # Sum_v W[j,v] * A[j,v]^2  -> length-n, replicate down rows to (n x n)
  cj <- rowSums(A2 * W)
  T2 <- matrix(rep(cj, each = n), nrow = n, ncol = n)

  # 2 * Sum_v W[j,v] * A[i,v] * A[j,v] -> 2 * (A %*% t(A * W))
  T3 <- 2 * (A %*% t(A * W))

  # Masked sum of squared diffs
  sumdiff2 <- T1 + T2 - T3

  # Mean the squared diffs by p_j (inside sqrt for RMSE)
  msd <- sweep(sumdiff2, 2, pvars, "/")

  # Root → RMSE
  D <- sqrt(msd)

  # Preserve row/col names when present
  if (!is.null(rownames(A))) {
    rownames(D) <- rownames(A)
    colnames(D) <- rownames(A)
  }
  D
}