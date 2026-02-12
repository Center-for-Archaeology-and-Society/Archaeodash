#' Main plotting function
#'
#' @param plotdf data frame for plotting
#' @param xvar x variable
#' @param yvar y variable
#' @param attrGroups attribute group
#' @param Conf whether to draw confidence ellipses
#' @param int.set confidence level
#' @param theme color theme
#'
#' @return plotly object
#' @export
#'
#' @examples
#' mainPlot(plotdf = rvals$plotdf,xvar = input$xvar,yvar = input$yvar,attrGroups = rvals$attrGroups,Conf = input$Conf, int.set = input$int.set)
mainPlot = function(plotdf, xvar, yvar, attrGroups, Conf, int.set, theme = "viridis"){
  message("main plot")
  i = 1
  # print(head(plotdf))
  # print(xvar)
  # print(yvar)
  # print(attrGroups)
  # print(Conf)
  # print(int.set)
  if(xvar == yvar) return(NULL)
  nfac = nlevels(plotdf[[attrGroups]])
  if(nfac > 6) mynotification("Too many groups to show symbols", type = "warning")

  if(nfac < 7) {
    print(i); i = i + 1
    gg = ggplot2::ggplot(
      plotdf,
      ggplot2::aes(
        x = !!as.name(xvar),
        y = !!as.name(yvar),
        shape = !!as.name(attrGroups),
        color = !!as.name(attrGroups),
        key = rowid
      )
    ) +
      ggplot2::geom_point()
  } else {
    print(i); i = i + 1
    gg = ggplot2::ggplot(
      plotdf,
      ggplot2::aes(
        x = !!as.name(xvar),
        y = !!as.name(yvar),
        color = !!as.name(attrGroups),
        key = rowid
      )
    ) +
      ggplot2::geom_point()
  }
  if(theme == "viridis"){
    gg = gg +
      ggplot2::scale_color_viridis_d()
  } else {
    groups = unique(plotdf[[attrGroups]])
    colors <- setNames(object = sapply(1:length(groups), function(x) {
      rgb(runif(1), runif(1), runif(1))
    }), nm = groups)
    gg = gg + ggplot2::scale_color_manual(values = colors)
  }
  if(Conf){
    print(i); i = i + 1
    gg = gg +
      ggplot2::stat_ellipse(
        data = plotdf,
        ggplot2::aes(
          x = !!as.name(xvar),
          y = !!as.name(yvar),
          color = !!as.name(attrGroups)
        ),
        inherit.aes = F,
        level = int.set,
        show.legend = F
      )
  }
  # Convert ggplot to plotly1
  print(i); i = i + 1
  pSymbolsF = function(x) {
    psymbols = c(
      "circle",
      "square",
      "diamond",
      "cross",
      "triangle-up",
      "hexagon"
    )
    if (length(unique(x)) > length(psymbols))
      psymbols = 'circle'
    dict = data.frame(x = unique(x), symbol = psymbols[1:length(unique(x))]) %>%
      dplyr::mutate(symbol = dplyr::case_when(is.na(symbol)~'circle',TRUE~symbol))
    x = data.frame(x = x) %>% dplyr::left_join(dict, by = "x")
    return(x$symbol)
  }
  print(i); i = i + 1
  # Extract the data used by ggplot
  gg_data <- ggplot2::ggplot_build(gg)$data[[1]] %>%
    dplyr::rename(rowid = key) %>%
    dplyr::left_join(plotdf %>% dplyr::select(tidyselect::all_of(c(
      'rowid', attrGroups
    ))), by = "rowid")  %>%
    dplyr::rename(
      name = !!as.name(attrGroups)
    ) %>%
    dplyr::mutate(
      shape = pSymbolsF(shape)
    ) %>%
    dplyr::mutate_at(dplyr::vars(colour,shape,name),factor)
  print(i); i = i + 1
  if(Conf){
    print(i); i = i + 1
    gg_data_ellipse <- ggplot2::ggplot_build(gg)$data[[2]]
    if("colour" %in% names(gg_data_ellipse)){
      gg_data_ellipse = gg_data_ellipse %>%
        dplyr::left_join(
          gg_data %>% dplyr::select(colour, name) %>% dplyr::distinct_all(),
          by = 'colour'
        ) %>%
        dplyr::mutate_at(dplyr::vars(colour,name),factor)
    } else {
      gg_data_ellipse = NULL
    }
  } else {
    gg_data_ellipse = NULL
  }
  print(i); i = i + 1
  # Add scatter plot trace
  ggP <-
    plotly::plot_ly() %>%
    plotly::add_trace(data = gg_data,
                      x = ~ x,
                      y = ~ y,
                      type = 'scatter',
                      mode = 'markers',
                      name = ~ name,
                      color = ~colour,
                      colors = ~levels(colour),
                      symbol = ~shape,
                      symbols = ~levels(shape),
                      legendgroup = ~ name,
                      key = ~ rowid,
                      marker = list(
                        size = ~ size * 7,
                        stroke = ~ stroke,
                        line = list(color = "black", width = .1)
                      ),
                      text = ~ glue::glue(
                        "{name}<br>rowid: {rowid}<br>{xvar}: {x}<br>{yvar}: {y}<br>"
                      ),
                      hoverinfo = 'text'
    )
  print(i); i = i + 1
  if(shiny::isTruthy(inherits(gg_data_ellipse,"data.frame"))){
    print(i); i = i + 1
    ggP = ggP %>%
      plotly::add_trace(
        data = gg_data_ellipse,
        x = ~ x,
        y = ~ y,
        legendgroup = ~name,
        type = 'scatter',
        mode = 'lines',
        name = ~ name,
        color = ~colour,
        colors = ~levels(colour),
        showlegend = F,
        line = list(
          color = ~colour
        ),
        inherit = F,
        hoverinfo = 'text',
        text = ~ glue::glue("{name}<br>color:{colour}")
      )
  }
  print(i); i = i + 1
  ggP = ggP %>%
    plotly::layout(
      xaxis = list(title = xvar),
      yaxis = list(title = yvar),
      dragmode = 'lasso'
    )
  print(i); i = i + 1
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
  message("multiplot")
  pdf1 = selectedData %>%
    dplyr::select(rowid,tidyselect::all_of(c(attrGroups,xvar))) %>%
    tidyr::pivot_longer(tidyselect::all_of(xvar), names_to = "xvar", values_to = "elem1") %>%
    tidyr::unite(id, c('rowid',tidyselect::all_of(attrGroups)), sep = "_", remove = F)
  pdf2 = selectedData %>%
    dplyr::select(rowid,tidyselect::all_of(c(attrGroups,yvar))) %>%
    tidyr::pivot_longer(tidyselect::all_of(yvar), names_to = "yvar", values_to = "elem2") %>%
    tidyr::unite(id, c('rowid',tidyselect::all_of(attrGroups)), sep = "_", remove = F)
  p = dplyr::full_join(pdf1,pdf2 %>% dplyr::select(-tidyselect::all_of(c('rowid',attrGroups))), by = 'id', relationship = "many-to-many") %>%
    dplyr::filter(elem1 != elem2) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = elem1,
      y = elem2,
      color = !!as.name(attrGroups),
      key = rowid
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
