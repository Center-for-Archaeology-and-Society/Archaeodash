#' Plot Missing Data
#'
#' From DataExplorer
#'
#' @param data elements
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_missing(data)
plot_missing = function (data){
  data = data %>%
    dplyr::mutate_all(quietly(as.numeric)) %>%
    dplyr::mutate_all(dplyr::na_if,y = 0)
  group = list(Good = 0.05, OK = 0.4, Bad = 0.8,
               Remove = 1)
  num_missing <- pct_missing <- Band <- NULL
  missing_value <- data.table(profile_missing(data))
  group <- group[sort.list(unlist(group))]
  invisible(lapply(seq_along(group), function(i) {
    if (i == 1) {
      missing_value[pct_missing <= group[[i]], `:=`(Band,
                                                    names(group)[i])]
    } else {
      missing_value[pct_missing > group[[i - 1]] & pct_missing <=
                      group[[i]], `:=`(Band, names(group)[i])]
    }
  }))
  output <- ggplot2::ggplot(missing_value, ggplot2::aes(x = feature,
                                             y = num_missing, fill = Band)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_discrete("Band") +
    ggplot2::coord_flip() +
    ggplot2::xlab("Features") +
    ggplot2::ylab("Missing Rows") +
    ggplot2::guides(fill = 'none') +
    # ggplot2::guides(fill = ggplot2::guide_legend(override.aes = ggplot2::aes(label = ""))) +
    # ggplot2::theme(legend.position = 'bottom') +
    ggplot2::theme_bw()
  return(output)
}

#' Profile Missing Data
#'
#' From DataExplorer
#'
#' @param data data to evalue
#'
#' @return data frame
#' @export
#'
#' @examples
#' profile_missing(data)
profile_missing = function (data){
  feature <- num_missing <- pct_missing <- group <- NULL
  is_data_table <- is.data.table(data)
  data_class <- class(data)
  if (!is_data_table)
    data <- data.table(data)
  missing_value <- data.table(feature = names(data), num_missing = sapply(data,
                                                                          function(x) {
                                                                            sum(is.na(x))
                                                                          }))
  missing_value[, `:=`(feature, factor(feature, levels = feature[order(-rank(num_missing))]))]
  missing_value[, `:=`(pct_missing, num_missing/nrow(data))][]
  if (!is_data_table)
    class(missing_value) <- data_class
  missing_value
}
