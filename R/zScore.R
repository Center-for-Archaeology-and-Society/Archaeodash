#' Convert to z score
#'
#' @param x vector of values
#'
#' @return data frame of z scores
#' @export
#'
#' @examples
#' zScore(x)
zScore = function(x){
  as.data.frame(scale(prop.table(as.matrix(x), 1) * 100)) %>% dplyr::mutate_all(round,3)
}
