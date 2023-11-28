#' Get Canonical Discriminant Analysis Results
#'
#' @param df data frame
#' @param chem  chemical element columns
#' @param attrGroups selected attribute
#'
#' @return list of results
#' @export
#'
#' @examples
#' getCDA(df,chem,attrGroups)
getCDA = function(df = rvals$selectedData, chem = rvals$chem, attrGroups = rvals$attrGroups){

  mdl = lm(as.formula(glue::glue("cbind({paste(chem,collapse = \",\")}) ~ {attrGroups}")), data = df)
  mdl_can = tryCatch(candisc::candisc(mdl, data = df),error = function(e){
    showNotification("unable to return CDA")
    return(NULL)
  })

  result = tryCatch(list(mod = mdl_can, CDAdf = dplyr::bind_cols(df %>% dplyr::select(-tidyselect::any_of(chem),-tidyselect::any_of(attrGroups)),mdl_can$scores)),error = function(e)return(NULL))
  return(result)
}
