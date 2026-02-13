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
  message("replacing data")
  tryCatch({
  for(df in c("importedData","selectedData","membershipProbs","edistance","pcadf","LDAdf","umapdf")){
    message(glue::glue("checking if {df} exists"))
    if (df == "membershipProbs"){
      colnm = "GroupVal"
    } else {
      colnm = col
    }
    if(isTruthy(!is.null(rvals[[df]]))){
      message(glue::glue("replacing {df} {colnm} with {paste(value,collapse = ',')}"))
    } else {
      message(glue::glue("{df} does not exist"))
      next
    }

    rvals[[df]]$rowid = as.character(rvals[[df]]$rowid)
    rowid = as.character(rowid)
    print("rowid")
    print(rowid)
    indx = which(rvals[[df]]$rowid %in% rowid)
    print("indx")
    print(indx)
    if(isTruthy(isFALSE(length(indx) == length(value) | length(value) == 1))){
      mynotification(glue::glue("length of value ({length(value)}) to replace does not match number of rows to replace ({length(indx)})"))
    } else {
      rvals[[df]][[colnm]] = as.character(rvals[[df]][[colnm]])
      rvals[[df]][[colnm]][indx] = as.character(value)
      rvals[[df]][[colnm]] = factor(rvals[[df]][[colnm]])
    }
  }
    print("updating current")
  rvals$attrGroupsSub = levels(rvals$selectedData[[col]])
  updateCurrent(rvals,
                con,
                credentials,
                input,
                output,
                session)
  }, error = function(e){
    mynotification(paste("Error saving results:\n",e),type = "error")
  })
}
