#' comp.profile
#'
#' plots concentrations
#'
#' @param x vector of concentrations
#' @param groups optional grouping vector; if provided, lines are colored by group
#'
#' @return plot
#' @export
#'
#' @examples
#' comp.profile(x)
comp.profile <- function(x, groups = NULL) {
  cols = names(x)
  plotdf = x %>%
    dplyr::mutate_all(quietly(as.numeric)) %>%
    tibble::rowid_to_column() %>%
    tidyr::pivot_longer(-rowid) %>%
    dplyr::mutate(name = factor(name, levels = cols))

  if (!is.null(groups) && length(groups) == nrow(x)) {
    plotdf = plotdf %>%
      dplyr::mutate(group_label = factor(rep(as.character(groups), each = length(cols))))
    p = plotdf %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = value, group = rowid, color = group_label)) +
      ggplot2::geom_line(alpha = .2)
  } else {
    p = plotdf %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = value, group = rowid)) +
      ggplot2::geom_line(color = "red", alpha = .1)
  }

  p +
    ggplot2::theme_bw() +
    ggplot2::ylab('Concentration (ppm)') +
    ggplot2::xlab('')
}


