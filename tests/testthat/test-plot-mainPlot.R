test_that("mainPlot works when rowid column is missing", {
  plotdf <- data.frame(
    grp = factor(c("A", "A", "B", "B")),
    x = c(1, 2, 3, 4),
    y = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )

  expect_no_error({
    p <- mainPlot(
      plotdf = plotdf,
      xvar = "x",
      yvar = "y",
      attrGroups = "grp",
      Conf = FALSE,
      int.set = 0.9,
      theme = "viridis"
    )
    expect_s3_class(p, "plotly")
  })
})

test_that("mainPlot works when rowid column is present", {
  plotdf <- data.frame(
    rowid = c("1", "2", "3", "4"),
    grp = factor(c("A", "A", "B", "B")),
    x = c(1, 2, 3, 4),
    y = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )

  expect_no_error({
    p <- mainPlot(
      plotdf = plotdf,
      xvar = "x",
      yvar = "y",
      attrGroups = "grp",
      Conf = FALSE,
      int.set = 0.9,
      theme = "viridis"
    )
    expect_s3_class(p, "plotly")
  })
})

test_that("mainPlot supports many groups with repeating symbols and optional labels", {
  set.seed(1)
  grp_vals <- paste0("G", seq_len(30))
  plotdf <- data.frame(
    rowid = as.character(seq_len(60)),
    grp = rep(grp_vals, each = 2),
    anid = paste0("AN", seq_len(60)),
    x = rnorm(60),
    y = rnorm(60),
    stringsAsFactors = FALSE
  )

  expect_no_error({
    p <- mainPlot(
      plotdf = plotdf,
      xvar = "x",
      yvar = "y",
      attrGroups = "grp",
      Conf = FALSE,
      int.set = 0.9,
      theme = "viridis",
      use_symbols = TRUE,
      show_point_labels = TRUE,
      label_col = "anid"
    )
    expect_s3_class(p, "plotly")
  })

  expect_no_error({
    p2 <- mainPlot(
      plotdf = plotdf,
      xvar = "x",
      yvar = "y",
      attrGroups = "grp",
      Conf = FALSE,
      int.set = 0.9,
      theme = "default",
      use_symbols = FALSE,
      show_point_labels = FALSE
    )
    expect_s3_class(p2, "plotly")
  })
})
