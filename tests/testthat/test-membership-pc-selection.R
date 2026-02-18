test_that("pc_columns_sorted orders principal component names numerically", {
  pc_columns_sorted <- getFromNamespace("pc_columns_sorted", "ArchaeoDash")
  cols <- c("PC10", "PC2", "PC1", "V1", "PC3")
  expect_equal(pc_columns_sorted(cols), c("PC1", "PC2", "PC3", "PC10"))
})

test_that("membership_pc_count_choices includes per-PC and cumulative variance labels", {
  membership_pc_count_choices <- getFromNamespace("membership_pc_count_choices", "ArchaeoDash")
  mat <- matrix(
    c(
      1, 2, 3,
      2, 3, 4,
      3, 4, 5,
      4, 5, 6,
      5, 6, 7
    ),
    ncol = 3,
    byrow = TRUE
  )
  pca_model <- stats::prcomp(mat, scale. = FALSE)

  choices <- membership_pc_count_choices(pca_model, c("PC1", "PC2", "PC3"))

  expect_equal(unname(choices), c("1", "2", "3"))
  expect_true(any(grepl("PC1:", names(choices), fixed = TRUE)))
  expect_true(any(grepl("cumulative:", names(choices), fixed = TRUE)))
})

test_that("membership_pc_count_choices falls back when PCA model is unavailable", {
  membership_pc_count_choices <- getFromNamespace("membership_pc_count_choices", "ArchaeoDash")
  choices <- membership_pc_count_choices(NULL, c("PC1", "PC2"))
  expect_equal(unname(choices), c("1", "2"))
  expect_equal(names(choices), c("First 1 PCs", "First 2 PCs"))
})

test_that("pc_axis_choices_with_variance includes per-PC variance labels", {
  pc_axis_choices_with_variance <- getFromNamespace("pc_axis_choices_with_variance", "ArchaeoDash")
  mat <- matrix(
    c(
      1, 2, 3,
      2, 3, 4,
      3, 4, 5,
      4, 5, 6,
      5, 6, 7
    ),
    ncol = 3,
    byrow = TRUE
  )
  pca_model <- stats::prcomp(mat, scale. = FALSE)
  choices <- pc_axis_choices_with_variance(pca_model, c("PC1", "PC2", "PC3"))

  expect_equal(unname(choices), c("PC1", "PC2", "PC3"))
  expect_true(any(grepl("^PC1 \\(", names(choices))))
})

test_that("limit_pc_features respects selected count and boundaries", {
  limit_pc_features <- getFromNamespace("limit_pc_features", "ArchaeoDash")
  pcs <- c("PC1", "PC2", "PC3", "PC4")
  expect_equal(limit_pc_features(pcs, 2), c("PC1", "PC2"))
  expect_equal(limit_pc_features(pcs, 999), pcs)
  expect_equal(limit_pc_features(pcs, NA), pcs)
})
