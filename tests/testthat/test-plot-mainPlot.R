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
