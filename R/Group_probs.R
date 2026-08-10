
#' Function for calculating group membership probabilities by chemical compositional distance using Mahalanobis distances and Hotellings T^2 statistic
#'
#' @param elements  transformed element data
#' @param assigned group designation by sample
#'
#' @return data frame
#' @export
#'
#' @examples
#' group.mem.probs(elements,assigned)
group.mem.probs <- function(data,chem,group,eligible,method = "Hotellings", ID) {
  probsAlldf = NULL
  eligible <- as.character(eligible)
  eligible <- eligible[!is.na(eligible) & nzchar(eligible)]
  eligible <- unique(eligible)
  if (length(eligible) == 0) {
    mynotification("No eligible groups are available for membership probabilities.", type = "error")
    return(NULL)
  }
  chem <- intersect(as.character(chem), names(data))
  if (length(chem) == 0) {
    mynotification("No valid analysis columns are available for membership probabilities.", type = "error")
    return(NULL)
  }
  if (identical(method, "Hotellings") && !app_require_packages("ICSNP", feature = "Hotellings T2 group probabilities")) {
    mynotification("Falling back to Mahalanobis because Hotellings T2 is unavailable.", type = "warning")
    return(group.mem.probs(data = data, chem = chem, group = group, eligible = eligible, method = "Mahalanobis", ID = ID))
  }
  probsAlldf <- tryCatch({
    if("PC1" %in% names(data)){
      chem = data %>% dplyr::select(tidyselect::any_of(tidyselect::contains("PC"))) %>% names()
    }

    probsAll = matrix(NA_real_, nrow = nrow(data), ncol = length(eligible))
    colnames(probsAll) = eligible
    rownames(probsAll) = as.character(data[[ID]])
    p.val <- NULL
    for (r in 1:nrow(data)) {
      for(grp in eligible){
        grpindx = which(data[[group]]==grp)
        grpindx = setdiff(grpindx,r)
        if(method == "Hotellings"){
          p.val <- tryCatch(
            ICSNP::HotellingsT2(data[r,chem],data[grpindx,chem])$p.value %>% round(.,5)*100,
            error = function(e) stop(e)
          )
        } else {
          p.val <- getMahalanobis(data[r,chem],data[grpindx,chem])
        }
        probsAll[r,which(eligible == grp)] <- if (is.finite(as.numeric(p.val))) as.numeric(p.val) else NA_real_
      }

    }
    if (identical(method, "Mahalanobis")) {
      probsAll[!is.finite(probsAll)] <- Inf
    }
    bg = getBestGroup(probsAll,eligible,method = method)
    if (length(bg$nms) != nrow(data)) bg$nms <- rep(NA_character_, nrow(data))
    if (length(bg$vals) != nrow(data)) bg$vals <- rep(NA_real_, nrow(data))
    probsAll %>%
      tibble::as_tibble() %>%
      dplyr::mutate(ID = as.character(data[[ID]]), Group = group, GroupVal = as.character(data[[group]]),BestGroup = bg$nms, BestValue = bg$vals,InGroup = GroupVal == BestGroup,.before = 1)
  }, error = function(e){
    if (identical(method, "Hotellings")) {
      mynotification(glue::glue("Hotellings failed ({e$message}); falling back to Mahalanobis."), type = "warning")
      return(group.mem.probs(data = data, chem = chem, group = group, eligible = eligible, method = "Mahalanobis", ID = ID))
    }
    mynotification(glue::glue("unable to return group membership probabilities: {e}"), type = "error")
    NULL
  })
  return(probsAlldf)
}

#' Calculate eligible groups
#'
#' number of rows must be 2 more than the number of columns per group
#'
#' @param group selected group attribute
#' @param data dataframe from imported data
#' @param chem names of columns with chemical elements
#'
#' @return vector of eligible groups for further analysis
#' @export
#'
#' @examples
#' getEligible(data,chem,group)
getEligible = function(data,chem,group){
  nc = length(chem)
  ng = length(unique(data[[group]]))
  m = max(nc,ng)
  eligible = data %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(group))) %>%
    dplyr::count() %>%
    dplyr::filter(n > (m + 1)) %>%
    dplyr::pull(!!as.name(group)) %>%
    as.character
  return(eligible)
}

#' Find best group based on largest values
#'
#' @param probsAll result from group probability
#' @param eligible groups
#'
#' @return vector of group names
#' @export
#'
#' @examples
#' getBestGroup(probsAll,eligible)
getBestGroup = function(probsAll,eligible, method){
  probsAll <- as.matrix(probsAll)
  if (!is.numeric(probsAll)) {
    suppressWarnings(storage.mode(probsAll) <- "double")
  }
  if (length(eligible) == 0 || nrow(probsAll) == 0 || ncol(probsAll) == 0) {
    return(list(nms = character(), vals = numeric()))
  }
  n <- nrow(probsAll)
  nms <- rep(NA_character_, n)
  val <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    row_vals <- probsAll[i, ]
    valid_idx <- which(!is.na(row_vals))
    if (length(valid_idx) == 0) next
    pick_idx <- if (method == "Hotellings") {
      valid_idx[which.max(row_vals[valid_idx])]
    } else {
      valid_idx[which.min(row_vals[valid_idx])]
    }
    nms[[i]] <- as.character(eligible[[pick_idx]])
    val[[i]] <- as.numeric(row_vals[[pick_idx]])
  }
  return(list(nms = nms, vals = val))
}

#' Get Mahalanobis distance
#'
#' @param row to get distance for
#' @param data dataset to compare against
#'
#' @return distance
#' @export
#'
#' @examples
#' getMahalanobis(data[r,],data[,data[[group]] == grp])
getMahalanobis = function(row, data){
  if (!is.data.frame(data) || nrow(data) < 2 || ncol(data) == 0) {
    return(Inf)
  }
  row_vec <- suppressWarnings(as.numeric(as.matrix(row)))
  data_matrix <- suppressWarnings(as.matrix(data))
  suppressWarnings(storage.mode(data_matrix) <- "double")
  if (length(row_vec) == 0 || ncol(data_matrix) == 0) {
    return(Inf)
  }
  keep_cols <- is.finite(row_vec)
  if (!any(keep_cols)) {
    return(Inf)
  }
  row_vec <- row_vec[keep_cols]
  data_matrix <- data_matrix[, keep_cols, drop = FALSE]
  if (ncol(data_matrix) == 0) {
    return(Inf)
  }
  complete_rows <- stats::complete.cases(data_matrix)
  data_matrix <- data_matrix[complete_rows, , drop = FALSE]
  if (nrow(data_matrix) < 2) {
    return(Inf)
  }
  variable_cols <- vapply(seq_len(ncol(data_matrix)), function(i) {
    vals <- data_matrix[, i]
    n_finite <- sum(is.finite(vals))
    if (n_finite < 2) return(FALSE)
    stats::var(vals, na.rm = TRUE) > 0
  }, logical(1))
  if (!any(variable_cols)) {
    return(Inf)
  }
  row_vec <- row_vec[variable_cols]
  data_matrix <- data_matrix[, variable_cols, drop = FALSE]
  if (ncol(data_matrix) == 0 || nrow(data_matrix) < 2) {
    return(Inf)
  }
  cov_matrix <- suppressWarnings(stats::cov(data_matrix))
  cov_matrix <- as.matrix(cov_matrix)
  if (any(!is.finite(cov_matrix))) {
    return(Inf)
  }
  mean_data <- suppressWarnings(colMeans(data_matrix))
  result <- tryCatch(
    stats::mahalanobis(matrix(row_vec, nrow = 1), mean_data, cov_matrix, tol = 1e-8)[[1]],
    error = function(e) {
      reg <- diag(1e-8, ncol(cov_matrix))
      tryCatch(
        stats::mahalanobis(matrix(row_vec, nrow = 1), mean_data, cov_matrix + reg, tol = 1e-8)[[1]],
        error = function(e2) Inf
      )
    }
  )
  if (is.finite(as.numeric(result))) as.numeric(result) else Inf
}
# read in sample data INAA_test, create attribute and element data.frames, impute missing data and transform
# mydat <- read.csv('inst/INAA_test.csv',header=T,row.names=1)
# attr1 <- mydat[,c(1,3,5,7)] # pull out attributes for plotting
# chem1 <- mydat[,c(8:21,23:40)] # pull out element data (excluing Ni)
# chem1[chem1==0] <- NA # set 0 values to NA
# chem.imp <- tidyr::complete(mice::mice(chem1,method='rf')) # impute missing data using the random forest approach
# chem.t <- log10(chem.imp) # log-base-10 transform raw element data
# grps <- unique(attr1$CORE)

# run script and view output as "kable"
# knitr::kable(
# test2 =group.mem.probs(chem.t,attr1$CORE,grps)
# )
#
#
# ## Parallel processing version below, in progress
#
# library(foreach)
# library(doParallel)
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
#
# probs <- list()
# for (m in 1:length(grps)) {
# probs[[m]] <-     cbind(foreach(i=1:nrow(chem.t[which(attr1$CORE==grps[m]),]),.combine='c',.packages='ICSNP') %dopar% (round((HotellingsT2(chem.t[which(attr1$CORE==grps[m]),][i,],chem.t[which(attr1$CORE==grps[m]),][-i,])$p.value),5)*100),
#                     foreach(j=1:length(grps[-m]),.combine=cbind,.packages='foreach') %:% foreach(i=1:nrow(x),.combine='c',.packages='ICSNP')
#                     %dopar% (HotellingsT2(x[i,],chem.t[which(attr1$CORE==grps[-m][j]),])$p.value) )
# probs[[m]] <- probs[[m]][]
# colnames(probs[[m]]) <- grps
# row.names(probs[[m]]) <- row.names(x)
# }
# proc.time()-ptm


