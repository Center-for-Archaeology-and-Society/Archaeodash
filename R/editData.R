#' Replace values
#'
#' @param rvals reactive values object
#' @param rowid rowid of value to change
#' @param col column of value to change
#' @param value new value
#'
#' @return reactive values object
#' @export
#'
#' @examples
#' replaceCell(rvals,rowid,col,value)
replaceCell = function(rowid, col, value, rvals, con, credentials, input,output,session) {
  for(df in c("importedData","selectedData","membershipProbs","edistance","pcadf","LDAdf")){
    if(isTRUE(is.null(rvals[[df]]))) next
    if (df == "membershipProbs"){
      colnm = "GroupVal"
    } else {
      colnm = col
    }
    message(glue::glue("replacing {df} {colnm} with {paste(value,collapse = ',')}"))
    indx = which(rvals[[df]]$rowid %in% rowid)
    if(isFALSE(length(indx) == length(value) | length(value) == 1)){
      mynotification(glue::glue("length of value ({length(value)}) to replace does not match number of rows to replace ({length(indx)})"))
    } else {
      rvals[[df]][[colnm]] = as.character(rvals[[df]][[colnm]])
      rvals[[df]][[colnm]][indx] = as.character(value)
      rvals[[df]][[colnm]] = factor(rvals[[df]][[colnm]])
    }
  }
  rvals$attrGroupsSub = levels(rvals$selectedData[[col]])
  updateCurrent(rvals,
                con,
                credentials,
                input,
                output,
                session)
}
