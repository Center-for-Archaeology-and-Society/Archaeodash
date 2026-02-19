#' Main plotting function
#'
#' @param plotdf data frame for plotting
#' @param xvar x variable
#' @param yvar y variable
#' @param attrGroups attribute group
#' @param Conf whether to draw confidence ellipses
#' @param int.set confidence level
#' @param theme color theme
#' @param use_symbols whether to draw group symbols
#' @param show_point_labels whether to show point labels
#' @param label_col column used for point labels
#'
#' @return plotly object
#' @export
#'
#' @examples
#' mainPlot(plotdf = rvals$plotdf,xvar = input$xvar,yvar = input$yvar,attrGroups = rvals$attrGroups,Conf = input$Conf, int.set = input$int.set)
mainPlot = function(plotdf, xvar, yvar, attrGroups, Conf, int.set, theme = "viridis", use_symbols = TRUE, show_point_labels = FALSE, label_col = NULL){
  if(xvar == yvar) return(NULL)
  if (!inherits(plotdf, "data.frame")) return(NULL)
  if (!(xvar %in% names(plotdf)) || !(yvar %in% names(plotdf)) || !(attrGroups %in% names(plotdf))) return(NULL)

  plot_data = plotdf
  if(!("rowid" %in% names(plot_data))){
    plot_data$rowid = seq_len(nrow(plot_data))
  }
  plot_data = plot_data %>% dplyr::mutate(.plot_key = as.character(rowid))
  group_values <- as.character(plot_data[[attrGroups]])
  group_values[is.na(group_values) | !nzchar(trimws(group_values))] <- "[NA]"
  group_levels <- sort(unique(group_values))
  plot_data$.plot_group <- factor(group_values, levels = group_levels)

  symbol_pool <- c(
    "circle", "square", "diamond", "cross", "x",
    "triangle-up", "triangle-down", "triangle-left", "triangle-right",
    "pentagon", "hexagon", "hexagon2", "star", "star-square",
    "star-diamond", "diamond-wide", "hourglass", "bowtie"
  )
  symbol_map <- stats::setNames(rep(symbol_pool, length.out = length(group_levels)), group_levels)
  plot_data$.plot_symbol <- as.character(symbol_map[as.character(plot_data$.plot_group)])
  plot_data$.plot_symbol[is.na(plot_data$.plot_symbol)] <- "circle"

  if (!is.character(label_col) || length(label_col) == 0 || is.na(label_col[[1]]) || !(label_col[[1]] %in% names(plot_data))) {
    label_col <- "rowid"
  } else {
    label_col <- label_col[[1]]
  }
  plot_data$.label_value <- as.character(plot_data[[label_col]])
  plot_data$.label_value[is.na(plot_data$.label_value)] <- ""
  plot_data$.hover_text <- glue::glue(
    "{as.character(plot_data$.plot_group)}<br>rowid: {plot_data$.plot_key}<br>{label_col}: {plot_data$.label_value}<br>{xvar}: {plot_data[[xvar]]}<br>{yvar}: {plot_data[[yvar]]}<br>"
  )

  gg = ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = !!as.name(xvar),
      y = !!as.name(yvar),
      color = .plot_group,
      text = .plot_key
    )
  ) +
    ggplot2::geom_point()

  theme_name <- if (!is.character(theme) || length(theme) == 0 || is.na(theme[[1]])) {
    "viridis"
  } else {
    as.character(theme[[1]])
  }

  palette_vals <- NULL
  if(identical(theme_name, "viridis")){
    gg = gg +
      ggplot2::scale_color_viridis_d()
    palette_vals <- grDevices::hcl.colors(length(group_levels), palette = "viridis")
    names(palette_vals) <- group_levels
  } else {
    if (length(group_levels) > 0) {
      palette_vals <- grDevices::hcl.colors(length(group_levels), palette = "Dark 3")
      names(palette_vals) <- group_levels
      gg = gg + ggplot2::scale_color_manual(values = palette_vals, drop = FALSE)
    }
  }

  ellipse_level <- suppressWarnings(as.numeric(int.set[[1]]))
  if (length(ellipse_level) == 0 || !is.finite(ellipse_level[[1]])) {
    ellipse_level <- 0.9
  } else {
    ellipse_level <- ellipse_level[[1]]
  }
  ellipse_level <- min(max(ellipse_level, 0.50), 0.99)

  gg_data_ellipse <- NULL
  if (isTRUE(Conf)) {
    ellipse_data <- plot_data %>%
      dplyr::mutate(
        .ellipse_x = suppressWarnings(as.numeric(.data[[xvar]])),
        .ellipse_y = suppressWarnings(as.numeric(.data[[yvar]]))
      ) %>%
      dplyr::filter(is.finite(.ellipse_x), is.finite(.ellipse_y))
    if (nrow(ellipse_data) > 0) {
      ellipse_data <- ellipse_data %>%
        dplyr::group_by(.plot_group) %>%
        dplyr::filter(dplyr::n() >= 3, dplyr::n_distinct(.ellipse_x) > 1, dplyr::n_distinct(.ellipse_y) > 1) %>%
        dplyr::ungroup()
      if (nrow(ellipse_data) > 0) {
        ellipse_gg <- ggplot2::ggplot(
          ellipse_data,
          ggplot2::aes(x = .ellipse_x, y = .ellipse_y, color = .plot_group)
        ) +
          ggplot2::stat_ellipse(level = ellipse_level, show.legend = FALSE)
        built_ellipse <- tryCatch(ggplot2::ggplot_build(ellipse_gg), error = function(e) NULL)
        if (!is.null(built_ellipse) && length(built_ellipse$data) > 0) {
          group_map <- ellipse_data %>%
            dplyr::transmute(.plot_group, .group_idx = as.integer(.plot_group)) %>%
            dplyr::distinct()
          gg_data_ellipse <- built_ellipse$data[[1]]
          if ("group" %in% names(gg_data_ellipse)) {
            gg_data_ellipse <- gg_data_ellipse %>%
              dplyr::mutate(.group_idx = as.integer(.data$group)) %>%
              dplyr::left_join(group_map, by = ".group_idx") %>%
              dplyr::rename(name = .plot_group)
          } else {
            gg_data_ellipse <- NULL
          }
        }
      }
    }
  }

  point_mode <- if (isTRUE(show_point_labels)) "markers+text" else "markers"
  ggP <- plotly::plot_ly()
  for (grp in group_levels) {
    grp_df <- plot_data %>% dplyr::filter(as.character(.plot_group) == grp)
    if (nrow(grp_df) == 0) next
    trace_symbol <- if (isTRUE(use_symbols)) grp_df$.plot_symbol else rep("circle", nrow(grp_df))
    trace_text <- if (isTRUE(show_point_labels)) grp_df$.label_value else NULL
    ggP <- ggP %>%
      plotly::add_trace(
        data = grp_df,
        x = as.formula(paste0("~", xvar)),
        y = as.formula(paste0("~", yvar)),
        type = "scatter",
        mode = point_mode,
        name = grp,
        legendgroup = grp,
        key = ~.plot_key,
        text = trace_text,
        textposition = "top center",
        textfont = list(size = 10),
        hovertext = ~.hover_text,
        hoverinfo = "text",
        marker = list(
          size = 7,
          symbol = trace_symbol,
          color = palette_vals[[grp]],
          line = list(color = "black", width = 0.1)
        )
      )
  }
  if (inherits(gg_data_ellipse, "data.frame") && nrow(gg_data_ellipse) > 0 && "name" %in% names(gg_data_ellipse)) {
    ellipse_groups <- unique(as.character(gg_data_ellipse$name))
    for (grp in ellipse_groups) {
      if (is.na(grp) || !nzchar(grp)) next
      grp_df <- gg_data_ellipse %>% dplyr::filter(as.character(name) == grp)
      if (nrow(grp_df) == 0) next
      line_col <- if ("colour" %in% names(grp_df)) as.character(grp_df$colour[[1]]) else palette_vals[[grp]]
      ggP <- ggP %>%
        plotly::add_trace(
          data = grp_df,
          x = ~x,
          y = ~y,
          legendgroup = grp,
          type = "scatter",
          mode = "lines",
          name = grp,
          showlegend = FALSE,
          line = list(color = line_col),
          inherit = FALSE,
          hoverinfo = "text",
          text = ~glue::glue("{grp} ellipse")
        )
    }
  }
  ggP = ggP %>%
    plotly::layout(
      xaxis = list(title = xvar),
      yaxis = list(title = yvar),
      dragmode = 'lasso'
    )
  ggP
}

#' Multiplot function
#'
#' @param selectedData data frame for plotting
#' @param attrGroups attribute group
#' @param xvar x variable
#' @param yvar y variable
#' @param ptsize point size
#' @param interactive whether to return plotly object or ggplot object
#'
#' @return plot object
#' @export
#'
#' @examples
#' multiplot(selectedData = rvals$selectedData,attrGroups = rvals$attrGroups,xvar  = input$xvar2, yvar = input$yvar2,ptsize = input$ptsize, interactive = input$interactive)
multiplot = function(selectedData,attrGroups, xvar, yvar, ptsize, interactive = F,theme){
  app_log("multiplot")
  plot_data = selectedData
  if(!("rowid" %in% names(plot_data))){
    plot_data$rowid = seq_len(nrow(plot_data))
  }

  pdf1 = plot_data %>%
    dplyr::select(rowid,tidyselect::all_of(c(attrGroups,xvar))) %>%
    tidyr::pivot_longer(tidyselect::all_of(xvar), names_to = "xvar", values_to = "elem1") %>%
    tidyr::unite(id, c('rowid',tidyselect::all_of(attrGroups)), sep = "_", remove = F)
  pdf2 = plot_data %>%
    dplyr::select(rowid,tidyselect::all_of(c(attrGroups,yvar))) %>%
    tidyr::pivot_longer(tidyselect::all_of(yvar), names_to = "yvar", values_to = "elem2") %>%
    tidyr::unite(id, c('rowid',tidyselect::all_of(attrGroups)), sep = "_", remove = F)
  p = dplyr::full_join(pdf1,pdf2 %>% dplyr::select(-tidyselect::all_of(c('rowid',attrGroups))), by = 'id', relationship = "many-to-many") %>%
    dplyr::filter(elem1 != elem2) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = elem1,
      y = elem2,
      color = !!as.name(attrGroups),
      text = rowid
    )) +
    ggplot2::geom_point(size = ptsize, alpha = .66) +
    ggplot2::ylab("") +
    ggplot2::theme_bw(base_size = 14,) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = '#404040'),
      strip.text = ggplot2::element_text(color = "white")
    )
  if(theme == "viridis"){
    p = p + ggplot2::scale_color_viridis_d()
  }
  if(length(xvar) > 1 || ((length(xvar) == 1 & length(yvar) == 1))){
    p = p +
      ggplot2::facet_grid(rows = dplyr::vars(yvar), cols = dplyr::vars(xvar), scales = 'free',switch = 'both') +
      ggplot2::xlab("")
  } else {
    p = p +
      ggplot2::facet_wrap(~yvar, scales = 'free',strip.position = "left") +
      ggplot2::xlab(xvar)
  }
  if(interactive){
    return(plotly::ggplotly(p))
  } else {
    return(p)
  }
}
