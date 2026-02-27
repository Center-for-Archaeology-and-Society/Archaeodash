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
  app_log("replacing data")
  tryCatch({
  ensure_core_rowids(rvals)
  assigned_values <- as.character(value)
  assigned_values <- assigned_values[!is.na(assigned_values) & nzchar(assigned_values)]
  for(df in c("importedData","selectedData","membershipProbs","edistance","pcadf","LDAdf","umapdf")){
    app_log(glue::glue("checking if {df} exists"))
    if (df == "membershipProbs"){
      colnm = "GroupVal"
    } else {
      colnm = col
    }
    if(isTruthy(!is.null(rvals[[df]]))){
      app_log(glue::glue("replacing {df} {colnm} with {paste(value,collapse = ',')}"))
    } else {
      app_log(glue::glue("{df} does not exist"))
      next
    }
    if(!("rowid" %in% names(rvals[[df]]))){
      ref <- NULL
      if (inherits(rvals$selectedData, "data.frame") && nrow(rvals[[df]]) == nrow(rvals$selectedData)) {
        ref <- rvals$selectedData$rowid
      }
      rvals[[df]] <- ensure_rowid_column(
        data = rvals[[df]],
        table_name = df,
        reference_rowid = ref,
        require_unique = !(df %in% c("membershipProbs", "edistance")),
        allow_long = df %in% c("membershipProbs", "edistance")
      )
    }
    if(!(colnm %in% names(rvals[[df]]))){
      app_log(glue::glue("{df} has no {colnm} column; skipping"))
      next
    }

    rvals[[df]]$rowid = as.character(rvals[[df]]$rowid)
    rowid = as.character(rowid)
    app_log("rowid")
    app_log(paste(rowid, collapse = ","))
    indx = which(rvals[[df]]$rowid %in% rowid)
    app_log("indx")
    app_log(paste(indx, collapse = ","))
    if(length(indx) == 0){
      app_log(glue::glue("no matching rows in {df}; skipping"))
      next
    }
    if(isTruthy(isFALSE(length(indx) == length(value) | length(value) == 1))){
      mynotification(glue::glue("length of value ({length(value)}) to replace does not match number of rows to replace ({length(indx)})"))
    } else {
      rvals[[df]][[colnm]] = as.character(rvals[[df]][[colnm]])
      rvals[[df]][[colnm]][indx] = as.character(value)
      rvals[[df]][[colnm]] = factor(rvals[[df]][[colnm]])
    }
  }
    app_log("updating current")
  if (inherits(rvals$selectedData, "data.frame") && col %in% names(rvals$selectedData)) {
    current_groups <- unique(as.character(rvals$selectedData[[col]]))
    current_groups <- current_groups[!is.na(current_groups) & nzchar(current_groups)]
    if (length(assigned_values) > 0) {
      current_groups <- unique(c(current_groups, assigned_values))
    }
    rvals$attrGroupsSub <- sort(current_groups)
  }
  rvals$transformations <- refreshTransformationMetadata(
    transformations = rvals$transformations,
    imported_data = rvals$importedData,
    chem = rvals$chem
  )
  if (!is.null(rvals$transformations) && length(rvals$transformations) > 0) {
    for (nm in names(rvals$transformations)) {
      snapshot <- rvals$transformations[[nm]]
      if (is.null(snapshot) || !identical(snapshot$attrGroups, col)) next
      snapshot_groups <- character()
      if (inherits(snapshot$selectedData, "data.frame") && col %in% names(snapshot$selectedData)) {
        snapshot_groups <- unique(as.character(snapshot$selectedData[[col]]))
        snapshot_groups <- snapshot_groups[!is.na(snapshot_groups) & nzchar(snapshot_groups)]
      }
      snapshot$attrGroupsSub <- sort(unique(c(as.character(snapshot$attrGroupsSub), snapshot_groups, assigned_values)))
      rvals$transformations[[nm]] <- snapshot
    }
  }
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
