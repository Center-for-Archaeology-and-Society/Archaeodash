# library(tidyverse)

# df = rio::import("C://Users/rjbischo/Downloads/INAA_ALL.csv")
# # df = rio::import("inst/app/INAA_data/cut1.xlsx")

# # df
# names(df)
# anid = df$ID
# m = df %>% select(la:v) %>%
# select(-ni) %>%
#  as.matrix()
# m = log10(m)
# d = dist(m, method = "euclidean")
# d = as.matrix(d)

# head(d)
# rownames(d) = anid
# colnames(d) = anid

# d[which(anid == "SRU002"),]

# euc_masked_single <- function(A) {
#   A <- as.matrix(A)
#   n <- nrow(A)

#   # Mask by "reference" row j: keep variables where A[j, ] != 0
#   W <- (A != 0) * 1.0              # n x p (numeric 0/1)
#   pvars <- rowSums(W)              # length n (per j)
#   pvars[pvars == 0] <- NA_real_    # avoid divide-by-zero; yields NA for empty masks

#   A2 <- A^2

#   # Sum_v w_{jv} * A_{iv}^2  --> (n x p) %*% (p x n) = (n x n)
#   T1 <- A2 %*% t(W)

#   # Sum_v w_{jv} * A_{jv}^2  --> length-n vector replicated down rows (n x n)
#   cj <- rowSums(A2 * W)
#   T2 <- matrix(rep(cj, each = n), nrow = n, ncol = n)

#   # 2 * Sum_v w_{jv} * A_{iv} * A_{jv} --> 2 * (A %*% t(A * W))  (n x n)
#   T3 <- 2 * (A %*% t(A * W))

#   # Masked squared-distance sums for all (i, j)
#   sumdiff2 <- T1 + T2 - T3

#   # Euclidean root and divide each column j by pvars[j]
#   D <- sqrt(sumdiff2)
#   D <- sweep(D, 2, pvars, "/")
#   return(round(D,4))
# }

# euc <- euc_masked_single(m)
# head(euc)
# rownames(euc) = anid
# colnames(euc) = anid

# euc[which(anid == "AID001"),which(anid == "GSC346")]

# top_n_per_row <- function(M, n = 10) {
#   apply(M, 1, function(row) {
#     # order indices by descending value
#     ord <- order(row, decreasing = TRUE)[1:min(n, length(row))]
#     data.frame(
#       column = colnames(M)[ord],
#       value  = row[ord],
#       stringsAsFactors = FALSE
#     )
#   })
# }

# topn = top_n_per_row(euc, n = 10)

# rmse <- function(actual, predicted) {
#   sqrt(mean((actual - predicted)^2, na.rm = TRUE))
# }
# rmse(m[1,],m[1223,])
# library(Metrics)

# Metrics::rmse(m[1,],m[1223,])

# # Pairwise RMSE with GAUSS-style masking:
# # D[i, j] = sqrt( sum_{v: A[j,v] != 0} (A[i,v] - A[j,v])^2 / p_j ),
# # where p_j = number of nonzero entries in row j.
# rmse_pairwise <- function(A) {
#   A <- as.matrix(A)
#   n <- nrow(A)

#   # Mask W[j, v] = 1 if A[j, v] != 0 else 0
#   W <- (A != 0) * 1.0
#   pvars <- rowSums(W)
#   pvars[pvars == 0] <- NA_real_  # No valid vars for that row ⇒ column becomes NA

#   A2 <- A^2

#   # Sum_v W[j,v] * A[i,v]^2  -> (n x p) %*% (p x n) = (n x n)
#   T1 <- A2 %*% t(W)

#   # Sum_v W[j,v] * A[j,v]^2  -> length-n, replicate down rows to (n x n)
#   cj <- rowSums(A2 * W)
#   T2 <- matrix(rep(cj, each = n), nrow = n, ncol = n)

#   # 2 * Sum_v W[j,v] * A[i,v] * A[j,v] -> 2 * (A %*% t(A * W))
#   T3 <- 2 * (A %*% t(A * W))

#   # Masked sum of squared diffs
#   sumdiff2 <- T1 + T2 - T3

#   # Mean the squared diffs by p_j (inside sqrt for RMSE)
#   msd <- sweep(sumdiff2, 2, pvars, "/")

#   # Root → RMSE
#   D <- sqrt(msd)

#   # Preserve row/col names when present
#   if (!is.null(rownames(A))) {
#     rownames(D) <- rownames(A)
#     colnames(D) <- rownames(A)
#   }
#   D
# }

# rmsem = rmse_pairwise_masked(m)
# rmsem[is.infinite(rmsem)] <- NA
# rmsem[which(anid == "AID001"),which(anid == "GSC346")]
# topn_rmse = top_n_per_row(rmsem, n = 10)

# plot(euc,rmsem)
# cor(euc,rmsem, na.rm = T)
