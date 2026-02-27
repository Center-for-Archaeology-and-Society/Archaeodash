test_that("refreshNonElementMetadata updates metadata while preserving element values", {
  imported <- tibble::tibble(
    rowid = c("1", "2"),
    GroupVal = c("A_new", "B_new"),
    site = c("S1", "S2"),
    Fe = c(100, 200)
  )
  transformed <- tibble::tibble(
    rowid = c("1", "2"),
    GroupVal = c("A_old", "B_old"),
    Fe = c(1.11, 2.22),
    transformation = c("log10", "log10")
  )

  refreshed <- refreshNonElementMetadata(
    transformed = transformed,
    imported_data = imported,
    chem = c("Fe")
  )

  expect_equal(refreshed$Fe, transformed$Fe)
  expect_equal(as.character(refreshed$GroupVal), c("A_new", "B_new"))
  expect_true("site" %in% names(refreshed))
  expect_equal(refreshed$transformation, transformed$transformation)
})

test_that("refreshNonElementMetadata can preserve selected metadata columns", {
  imported <- tibble::tibble(
    rowid = c("1", "2"),
    GroupVal = c("A_new", "B_new"),
    site = c("S1", "S2"),
    Fe = c(100, 200)
  )
  transformed <- tibble::tibble(
    rowid = c("1", "2"),
    GroupVal = c("A_old", "B_old"),
    Fe = c(1.11, 2.22)
  )

  refreshed <- refreshNonElementMetadata(
    transformed = transformed,
    imported_data = imported,
    chem = c("Fe"),
    preserve_cols = "GroupVal"
  )

  expect_equal(refreshed$Fe, transformed$Fe)
  expect_equal(as.character(refreshed$GroupVal), c("A_old", "B_old"))
  expect_true("site" %in% names(refreshed))
})

test_that("refreshTransformationMetadata updates all stored transformed tables", {
  imported <- tibble::tibble(
    rowid = c("1", "2", "3"),
    GroupVal = c("A_new", "B_new", "A_new"),
    site = c("S1", "S2", "S3"),
    Fe = c(100, 200, 300)
  )
  snapshot <- list(
    name = "t1",
    attrGroups = "GroupVal",
    attrGroupsSub = "A_new",
    chem = c("Fe"),
    selectedDataAll = tibble::tibble(rowid = c("1", "2", "3"), GroupVal = c("A_old", "B_old", "A_old"), Fe = c(1, 2, 3)),
    selectedData = tibble::tibble(rowid = c("1", "3"), GroupVal = c("A_old", "A_old"), Fe = c(1, 3)),
    pcadf = tibble::tibble(rowid = c("1", "2"), GroupVal = c("A_old", "B_old"), PC1 = c(0.1, 0.2)),
    umapdf = tibble::tibble(rowid = c("1", "2"), GroupVal = c("A_old", "B_old"), V1 = c(0.3, 0.4)),
    LDAdf = tibble::tibble(rowid = c("1", "2"), GroupVal = c("A_old", "B_old"), LD1 = c(0.5, 0.6))
  )

  refreshed <- refreshTransformationMetadata(
    transformations = list(t1 = snapshot),
    imported_data = imported,
    chem = c("Fe")
  )

  expect_equal(as.character(refreshed$t1$selectedDataAll$GroupVal), c("A_new", "B_new", "A_new"))
  expect_equal(sort(as.character(refreshed$t1$selectedData$rowid)), c("1", "3"))
  expect_equal(as.character(refreshed$t1$selectedData$GroupVal), c("A_new", "A_new"))
  expect_equal(as.character(refreshed$t1$pcadf$GroupVal), "A_new")
  expect_equal(as.character(refreshed$t1$umapdf$GroupVal), "A_new")
  expect_equal(as.character(refreshed$t1$LDAdf$GroupVal), "A_new")
})

test_that("build and apply transformation snapshot round-trip core fields", {
  rvals <- new.env(parent = emptyenv())
  rvals$chem <- c("Fe")
  rvals$attr <- c("GroupVal")
  rvals$attrs <- c("GroupVal")
  rvals$attrGroups <- "GroupVal"
  rvals$attrGroupsSub <- c("A")
  rvals$transform.method <- "log10"
  rvals$impute.method <- "none"
  rvals$runPCA <- TRUE
  rvals$runUMAP <- FALSE
  rvals$runLDA <- FALSE
  rvals$selectedData <- tibble::tibble(rowid = "1", GroupVal = "A", Fe = 1)
  rvals$pcadf <- tibble::tibble(rowid = "1", GroupVal = "A", PC1 = 0.1)
  rvals$umapdf <- tibble::tibble()
  rvals$LDAdf <- tibble::tibble()
  rvals$pca <- list(sdev = 1)
  rvals$LDAmod <- NULL

  snapshot <- buildTransformationSnapshot(rvals, "my_transform")

  rvals2 <- new.env(parent = emptyenv())
  applyTransformationSnapshot(rvals2, snapshot)

  expect_equal(rvals2$activeTransformation, "my_transform")
  expect_equal(rvals2$chem, c("Fe"))
  expect_equal(rvals2$attrGroups, "GroupVal")
  expect_equal(rvals2$transform.method, "log10")
  expect_equal(rvals2$selectedData$Fe, 1)
})
