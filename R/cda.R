#' Get Canonical Discriminant Analysis Results
#'
#' @param df data frame
#' @param chem  chemical element columns
#' @param attrGroups selected attribute
#'
#' @return
#' @export
#'
#' @examples
getCDA = function(df = rvals$selectedData, chem = rvals$chem, attrGroups = rvals$attrGroups){

  mdl = lm(as.formula(glue::glue("cbind({paste(chem,collapse = \",\")}) ~ {attrGroups}")), data = df)
  mdl_can = candisc::candisc(mdl, data = df)
  return(list(mod = mdl_can, CDAdf = dplyr::left_join(rvals$selectedData %>% dplyr::select(-tidyselect::any_of(chem)),mdl_can$scores %>% dplyr::mutate(rowid = 1:dplyr::n()) %>% dplyr::select(-tidyselect::any_of(attrGroups)), by = "rowid")))
}
