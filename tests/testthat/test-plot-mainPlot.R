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

test_that("mainPlot uses conservative marker symbols for cross-version compatibility", {
  set.seed(2)
  grp_vals <- paste0("G", seq_len(25))
  plotdf <- data.frame(
    rowid = as.character(seq_len(75)),
    grp = rep(grp_vals, each = 3),
    x = rnorm(75),
    y = rnorm(75),
    stringsAsFactors = FALSE
  )

  p <- mainPlot(
    plotdf = plotdf,
    xvar = "x",
    yvar = "y",
    attrGroups = "grp",
    Conf = FALSE,
    int.set = 0.9,
    theme = "viridis",
    use_symbols = TRUE
  )

  built <- plotly::plotly_build(p)
  marker_symbols <- unlist(lapply(built$x$data, function(tr) tr$marker$symbol), use.names = FALSE)
  marker_symbols <- unique(as.character(marker_symbols))
  marker_symbols <- marker_symbols[!is.na(marker_symbols) & nzchar(marker_symbols)]

  expect_true(length(marker_symbols) > 0)
  expect_true(all(marker_symbols %in% c(
    "circle", "square", "diamond", "cross", "x",
    "triangle-up", "triangle-down", "triangle-left", "triangle-right", "star"
  )))
})

test_that("mainPlot tolerates missing theme and ellipse level inputs", {
  plotdf <- data.frame(
    rowid = as.character(seq_len(10)),
    grp = rep(c("A", "B"), each = 5),
    V1 = rnorm(10),
    V2 = rnorm(10),
    stringsAsFactors = FALSE
  )

  expect_no_error({
    p <- mainPlot(
      plotdf = plotdf,
      xvar = "V1",
      yvar = "V2",
      attrGroups = "grp",
      Conf = TRUE,
      int.set = NULL,
      theme = NULL
    )
    expect_s3_class(p, "plotly")
  })
})

test_that("mainPlot skips ellipse gracefully when data are not ellipse-eligible", {
  plotdf <- data.frame(
    rowid = as.character(seq_len(6)),
    grp = c("A", "A", "B", "B", "C", "C"),
    V1 = c("1", "1", "2", "2", "3", "3"),
    V2 = c("4", "4", "5", "5", "6", "6"),
    stringsAsFactors = FALSE
  )

  expect_no_error({
    p <- mainPlot(
      plotdf = plotdf,
      xvar = "V1",
      yvar = "V2",
      attrGroups = "grp",
      Conf = TRUE,
      int.set = 0.9,
      theme = "viridis"
    )
    expect_s3_class(p, "plotly")
  })
})

test_that("multiplot returns expected class for interactive and static modes", {
  set.seed(3)
  selectedData <- data.frame(
    rowid = as.character(seq_len(12)),
    grp = rep(c("A", "B"), each = 6),
    V1 = rnorm(12),
    V2 = rnorm(12),
    V3 = rnorm(12),
    stringsAsFactors = FALSE
  )

  p_static <- multiplot(
    selectedData = selectedData,
    attrGroups = "grp",
    xvar = c("V1", "V2"),
    yvar = c("V2", "V3"),
    ptsize = 2,
    interactive = FALSE,
    theme = "viridis"
  )
  expect_s3_class(p_static, "ggplot")

  p_interactive <- multiplot(
    selectedData = selectedData,
    attrGroups = "grp",
    xvar = c("V1", "V2"),
    yvar = c("V2", "V3"),
    ptsize = 2,
    interactive = TRUE,
    theme = "viridis"
  )
  expect_s3_class(p_interactive, "plotly")
})
